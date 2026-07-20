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
%% @doc Compatibility stubs for the Erlang/OTP net_adm interface.
%% @end
%%-----------------------------------------------------------------------------
-module(net_adm).

-export([
    ping/1
]).

%%-----------------------------------------------------------------------------
%% @param   Node the node to ping
%% @returns `pang'
%% @doc Compatibility stub; not supported on AtomVM. Establishing a
%% connection by pinging is not implemented, so this always returns `pang'.
%% @end
%%-----------------------------------------------------------------------------
-spec ping(Node :: node()) -> pong | pang.
ping(_Node) ->
    pang.
