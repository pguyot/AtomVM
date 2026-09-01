%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

%%-----------------------------------------------------------------------------
%% @doc An implementation of a subset of the Erlang/OTP erts_debug interface.
%% @end
%%-----------------------------------------------------------------------------
-module(erts_debug).

-export([flat_size/1, size_shared/1]).

%%-----------------------------------------------------------------------------
%% @param   Term        term to get the size of
%% @returns A size
%% @doc     Return the size, in terms, of a given term.
%% @end
%%-----------------------------------------------------------------------------
-spec flat_size(Term :: any()) -> non_neg_integer().
flat_size(_Term) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Term        term to get the size of
%% @returns A size
%% @doc     Return the size, in terms, of a given term, counting subterms that
%%          are shared within it only once.
%%
%%          AtomVM does not preserve sharing: every term copy (message send,
%%          ETS read, `binary_to_term/1', ...) expands shared subterms, so this
%%          function currently returns the same value as {@link flat_size/1},
%%          which is the size the term takes once copied.
%% @end
%%-----------------------------------------------------------------------------
-spec size_shared(Term :: any()) -> non_neg_integer().
size_shared(_Term) ->
    erlang:nif_error(undefined).
