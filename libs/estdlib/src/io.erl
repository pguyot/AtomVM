%
% This file is part of AtomVM.
%
% Copyright 2018-2021 Davide Bettio <davide@uninstall.it>
% Copyright 2019 Fred Dushin <fred@dushin.net>
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
%% @doc An implementation of the Erlang/OTP io interface.
%%
%% This module implements a strict subset of the Erlang/OTP io interface.
%% @end
%%-----------------------------------------------------------------------------
-module(io).

-export([requests/1, format/1, format/2, fwrite/2, get_line/1, put_chars/1, put_chars/2, scan_erl_exprs/4, columns/0, getopts/0, printable_range/0]).

requests(Requests) ->
    execute_request(group_leader(), {requests, Requests}).

columns() ->
    {ok, 80}.

% sensible default so far
getopts() ->
    [
        {echo,true},
        {binary,false},
        {encoding,unicode},
        {terminal,true},
        {stdout,true},
        {stderr,true},
        {stdin,true}
    ].

printable_range() ->
    unicode.

fwrite(Format, Data) ->
    format(Format, Data).

scan_erl_exprs(Device, Prompt, StartLocation, Options) when is_pid(Device) ->
    execute_request(Device, {get_until, unicode, Prompt, erl_scan, tokens, [StartLocation, Options]}).

execute_request(Pid, Request) ->
    Converted = convert_request(Request),
    Ref = make_ref(),
    Pid ! {io_request, self(), Ref, Converted},
    receive
        {io_reply, Ref, Result} -> Result
    end.

%%-----------------------------------------------------------------------------
%% @doc     Equivalent to format(Format, []).
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format :: string()) -> string().
format(Format) when is_list(Format) ->
    format(Format, []).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format argument
%% @returns string
%% @doc     Format string and data to console.
%%          See io_lib:format/2 for information about
%%          formatting capabilities.
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format :: string(), Args :: list()) -> string().
format(Format, Args) when is_list(Format) andalso is_list(Args) ->
    Msg =
        try
            io_lib:format(Format, Args)
        catch
            _:_ ->
                io_lib:format("Bad format!  Format: ~p Args: ~p~n", [Format, Args])
        end,
    put_chars(Msg).

%%-----------------------------------------------------------------------------
%% @param   Prompt prompt for user input
%% @returns string
%% @doc     Read string from console with prompt.
%% @end
%%-----------------------------------------------------------------------------
-spec get_line(Prompt :: string()) -> string().
get_line(Prompt) ->
    Self = self(),
    case erlang:group_leader() of
        Self ->
            erlang:throw(no_group_leader);
        Leader ->
            Ref = make_ref(),
            Leader ! {io_request, self(), Ref, {get_line, unicode, Prompt}},
            receive
                {io_reply, Ref, Line} -> Line
            end
    end.

%%-----------------------------------------------------------------------------
%% @param   Chars character(s) to write to console
%% @returns ok
%% @doc     Writes the given character(s) to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec put_chars(Chars :: list() | binary()) -> ok.
put_chars(Chars) ->
    Self = self(),
    case erlang:group_leader() of
        Self ->
            console:print(Chars);
        Leader ->
            execute_request(Leader, {put_chars, unicode, Chars})
    end.

%%-----------------------------------------------------------------------------
%% @param   Chars character(s) to write to console
%% @returns ok
%% @doc     Writes the given character(s) to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec put_chars(Device :: any(), Chars :: list() | binary()) -> ok.
put_chars(standard_error, Chars) ->
    put_chars(Chars);
put_chars(standard_output, Chars) ->
    put_chars(Chars).

convert_request({requests, Requests}) ->
    {requests, [convert_request(Request) || Request <- Requests]};
convert_request(nl) ->
    {put_chars, unicode, "\n"};
convert_request(Other) ->
    Other.
