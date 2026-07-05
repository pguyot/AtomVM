%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% Shared write-through x-register store elision (the "pending" mechanism).
%%
%% Every store to ctx->x[N] is emitted normally (write-through) and then
%% RECORDED as pending, with its byte offset in the stream, its width and
%% the conditional-block depth. When a new store to the same slot is
%% emitted at the same depth and nothing between could have observed the
%% slot in memory, the earlier store is rewritten to a same-width nop: all
%% intermediate consumers provably read the value from the register cache.
%%
%% Because the store is emitted first and only later neutralized in place,
%% the mechanism is FAIL-SAFE: a missing hook merely leaves a store that
%% could have been elided, never a stale slot.
%%
%% Observation points drop pendings (they keep their store):
%%   - a memory read of the slot (pending_clear_x),
%%   - any call, since the callee reads ctx->x (pending_clear_all),
%%   - a label, an unknown predecessor could read the slot
%%     (pending_flush_label: nops the ones not live-in, keeps the rest),
%%   - a branch to a label whose live-in mask contains the register
%%     (pending_filter_label).
%% Conditional bodies only elide stores made at their own depth
%% (pending_enter_cond / pending_exit_cond).
%%
%% Backends including this file must provide:
%%   - `#state{}' with fields `pending_x', `cond_depth', `live_masks',
%%     `stream', `stream_module',
%%   - `pending_nop_bytes/1' : a binary of N bytes of nop(s) for a given
%%     store width N.
%% The pending map is `#{Xreg => {Offset, Width, Depth}}'.
%% Elision is inert until live_masks are set (set_live_masks/2); per-op
%% emission tests then keep plain write-through behavior.

pending_clear_all(#state{pending_x = P} = State) when map_size(P) =:= 0 ->
    State;
pending_clear_all(State) ->
    State#state{pending_x = #{}}.

pending_clear_x(#state{pending_x = P} = State, X) when is_integer(X) ->
    case is_map_key(X, P) of
        true -> State#state{pending_x = maps:remove(X, P)};
        false -> State
    end;
pending_clear_x(State, _X) ->
    State.

%% Before emitting a store to x[X]: if a same-depth pending store exists,
%% it is dead — nop it out.
pending_elide_prev(
    #state{pending_x = P, cond_depth = D, stream_module = SM, stream = St0} = State, X
) ->
    case P of
        #{X := {Off, Width, D}} ->
            State#state{stream = SM:replace(St0, Off, pending_nop_bytes(Width))};
        _ ->
            State
    end.

%% After emitting a store to x[X]: StartOffset is the stream offset just
%% before the store was appended, so the store occupies
%% [StartOffset, current offset) and its width is derived from the stream.
%% This keeps the mechanism agnostic to variable-width store encodings
%% (e.g. x86-64's disp8 vs disp32 forms).
pending_note_store(#state{live_masks = undefined} = State, _X, _StartOffset) ->
    State;
pending_note_store(
    #state{pending_x = P, cond_depth = D, stream_module = SM, stream = St} = State, X, StartOffset
) when is_integer(X), X < 32 ->
    Width = SM:offset(St) - StartOffset,
    State#state{pending_x = P#{X => {StartOffset, Width, D}}};
pending_note_store(State, _X, _StartOffset) ->
    State.

%% Branch to a label: pendings whose register is in the target's live-in
%% mask must keep their store (the target may read it from memory).
pending_filter_label(#state{live_masks = undefined} = State, _Label) ->
    State;
pending_filter_label(#state{pending_x = P} = State, _Label) when map_size(P) =:= 0 ->
    State;
pending_filter_label(#state{pending_x = P, live_masks = Masks} = State, Label) ->
    Mask = maps:get(Label, Masks, -1),
    State#state{
        pending_x = maps:filter(fun(X, _) -> Mask band (1 bsl X) =:= 0 end, P)
    }.

%% Window end at a label: pendings that survived every branch filter and
%% whose register is not in the label's live-in mask are fully dead — nop
%% their stores. Registers in the mask keep their stores. Either way the
%% tracking window ends here.
pending_flush_label(#state{live_masks = undefined} = State, _Label) ->
    State;
pending_flush_label(#state{pending_x = P} = State, _Label) when map_size(P) =:= 0 ->
    State;
pending_flush_label(
    #state{pending_x = P, live_masks = Masks} = State, Label
) ->
    Mask = maps:get(Label, Masks, -1),
    Stream1 = maps:fold(
        fun(X, {Off, Width, _D}, StAcc) ->
            case Mask band (1 bsl X) of
                0 -> (State#state.stream_module):replace(StAcc, Off, pending_nop_bytes(Width));
                _ -> StAcc
            end
        end,
        State#state.stream,
        P
    ),
    State#state{stream = Stream1, pending_x = #{}}.

pending_enter_cond(#state{cond_depth = D} = State) ->
    State#state{cond_depth = D + 1}.

pending_exit_cond(#state{cond_depth = D, pending_x = P} = State) ->
    D1 = D - 1,
    State#state{
        cond_depth = D1,
        pending_x = maps:filter(fun(_, {_, _, PD}) -> PD =< D1 end, P)
    }.
