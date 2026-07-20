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
%% @doc An implementation of a small subset of the Erlang/OTP error_logger
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(error_logger).

-export([
    error_msg/2
]).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format arguments
%% @returns `ok'
%% @doc     Log an error message. On AtomVM the message is simply printed to
%%          the standard error device.
%% @end
%%-----------------------------------------------------------------------------
-spec error_msg(Format :: string(), Args :: [term()]) -> ok.
error_msg(Format, Args) ->
    io:format(standard_error, Format, Args).
