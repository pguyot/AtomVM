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
%% @doc Compatibility stubs for the Erlang/OTP global interface.
%% @end
%%-----------------------------------------------------------------------------
-module(global).

-export([
    send/2,
    whereis_name/1
]).

%%-----------------------------------------------------------------------------
%% @param   Name the globally registered name to send the message to
%% @param   Msg the message to send
%% @returns the pid of the process the message was sent to
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec send(Name :: term(), Msg :: term()) -> pid().
send(_Name, _Msg) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Name the globally registered name to look up
%% @returns the pid of the process, or `undefined'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec whereis_name(Name :: term()) -> pid() | undefined.
whereis_name(_Name) ->
    erlang:nif_error(undefined).
