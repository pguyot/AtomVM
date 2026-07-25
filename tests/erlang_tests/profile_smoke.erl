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

-module(profile_smoke).
-export([start/0]).

%% Regression test for atomvm:profile_start/0, atomvm:profile_stop/0 (see
%% msacc.h/msacc.c): a CPU-bound loop should report nonzero emulator time and
%% at least one hotspot in this module, a GC-heavy loop should report nonzero
%% gc time, and every state name/count and hotspot entry must be well-formed.
start() ->
    ok = atomvm:profile_start(),
    ok = burn(200000),
    ok = gc_loop(5000, []),
    Report = atomvm:profile_stop(),
    true = length(Report) > 0,
    ok = check_schedulers(Report),
    %% This process ran a long CPU-bound loop and a GC-heavy loop on some
    %% scheduler thread, so summed across every scheduler, emulator and gc
    %% time must both be real and nonzero, and at least one hotspot in this
    %% module must have been sampled.
    true = sum_state(emulator, Report) > 0,
    true = sum_state(gc, Report) > 0,
    true = sum_hotspots_in(?MODULE, Report) > 0,
    0.

sum_state(Name, Report) ->
    sum_state(Name, Report, 0).

sum_state(_Name, [], Acc) ->
    Acc;
sum_state(Name, [{_SchedIdx, States, _Hotspots} | Rest], Acc) ->
    sum_state(Name, Rest, Acc + get_state_ns(Name, States)).

sum_hotspots_in(Module, Report) ->
    sum_hotspots_in(Module, Report, 0).

sum_hotspots_in(_Module, [], Acc) ->
    Acc;
sum_hotspots_in(Module, [{_SchedIdx, _States, Hotspots} | Rest], Acc) ->
    sum_hotspots_in(Module, Rest, Acc + sum_hotspots_for_module(Module, Hotspots)).

sum_hotspots_for_module(_Module, []) ->
    0;
sum_hotspots_for_module(Module, [{{Module, _Function, _Arity, _Line}, Count} | Rest]) ->
    Count + sum_hotspots_for_module(Module, Rest);
sum_hotspots_for_module(Module, [_ | Rest]) ->
    sum_hotspots_for_module(Module, Rest).

check_schedulers([]) ->
    ok;
check_schedulers([{SchedIdx, States, Hotspots} | Rest]) ->
    true = is_integer(SchedIdx) andalso SchedIdx >= 0,
    ok = check_states(States),
    ok = check_hotspots(Hotspots),
    check_schedulers(Rest).

check_states(States) ->
    4 = length(States),
    true = get_state_ns(emulator, States) >= 0,
    true = get_state_ns(gc, States) >= 0,
    true = get_state_ns(scheduler, States) >= 0,
    true = get_state_ns(sleep, States) >= 0,
    check_state_names(States).

check_state_names([]) ->
    ok;
check_state_names([{Name, Ns} | Rest]) ->
    true = lists_member(Name, [emulator, gc, scheduler, sleep]),
    true = is_integer(Ns),
    check_state_names(Rest).

get_state_ns(Name, States) ->
    case lists_keyfind(Name, 1, States) of
        {Name, Ns} -> Ns;
        false -> -1
    end.

check_hotspots([]) ->
    ok;
check_hotspots([{{Module, Function, Arity, Line}, Count} | Rest]) ->
    true = is_atom(Module),
    true = Function =:= undefined orelse is_atom(Function),
    true = is_integer(Arity),
    true = is_integer(Line) andalso Line >= 0,
    true = is_integer(Count) andalso Count > 0,
    check_hotspots(Rest).

lists_member(_X, []) ->
    false;
lists_member(X, [X | _T]) ->
    true;
lists_member(X, [_ | T]) ->
    lists_member(X, T).

lists_keyfind(_Key, _N, []) ->
    false;
lists_keyfind(Key, N, [Tuple | Rest]) ->
    case element(N, Tuple) of
        Key -> Tuple;
        _ -> lists_keyfind(Key, N, Rest)
    end.

burn(0) ->
    ok;
burn(N) ->
    _ = N rem 7 + N rem 5 + N rem 3,
    burn(N - 1).

gc_loop(0, _Acc) ->
    ok;
gc_loop(N, Acc) ->
    NewAcc = [make_list(20, N, []) | Acc],
    Trimmed = trim(NewAcc, 50),
    gc_loop(N - 1, Trimmed).

make_list(0, _V, Acc) ->
    Acc;
make_list(N, V, Acc) ->
    make_list(N - 1, V, [V | Acc]).

trim(L, Max) ->
    trim(L, Max, length(L)).

trim(L, Max, Len) when Len =< Max ->
    L;
trim([_ | T], Max, Len) ->
    trim(T, Max, Len - 1).
