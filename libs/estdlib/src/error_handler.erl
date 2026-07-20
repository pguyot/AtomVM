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

%%-----------------------------------------------------------------------------
%% @doc Compatibility stubs for the Erlang/OTP error_handler interface.
%% @end
%%-----------------------------------------------------------------------------
-module(error_handler).

-export([
    undefined_function/3,
    undefined_lambda/3
]).

%%-----------------------------------------------------------------------------
%% @param   Module module of the undefined function
%% @param   Function name of the undefined function
%% @param   Args arguments of the call
%% @returns the value of the call, if the function can be loaded
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec undefined_function(Module :: module(), Function :: atom(), Args :: [any()]) -> any().
undefined_function(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module module of the fun
%% @param   Fun the fun that was called
%% @param   Args arguments of the call
%% @returns the value of the call, if the module can be loaded
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec undefined_lambda(Module :: module(), Fun :: function(), Args :: [any()]) -> any().
undefined_lambda(_Module, _Fun, _Args) ->
    erlang:nif_error(undefined).
