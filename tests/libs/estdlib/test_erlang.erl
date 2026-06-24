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

-module(test_erlang).

-export([test/0]).

test() ->
    ok = test_md5(),
    ok = test_standard_error(),
    ok = test_undef_names_mfa(),
    ok = test_statistics(),
    ok.

test_statistics() ->
    {Total1, _Since1} = erlang:statistics(wall_clock),
    true = is_integer(Total1) andalso Total1 >= 0,
    %% Busy-wait a little so the monotonic clock advances, then check the delta.
    ok = spin_until_advanced(),
    {Total2, Since2} = erlang:statistics(wall_clock),
    true = Total2 >= Total1,
    true = is_integer(Since2) andalso Since2 >= 0,
    {RTotal, _RSince} = erlang:statistics(runtime),
    true = is_integer(RTotal) andalso RTotal >= 0,
    ok.

spin_until_advanced() ->
    Start = erlang:monotonic_time(millisecond),
    spin_until_advanced(Start).

spin_until_advanced(Start) ->
    case erlang:monotonic_time(millisecond) > Start of
        true -> ok;
        false -> spin_until_advanced(Start)
    end.

%% Calling a function AtomVM cannot resolve (here, an unknown module) must raise
%% undef whose stacktrace names the real Module:Function/Arity -- not the
%% caller's module with undefined/0, which masked the actual missing function.
test_undef_names_mfa() ->
    Stk =
        try atomvm_no_such_module:no_such_fun(1, 2, 3) of
            _ -> error(should_have_failed)
        catch
            error:undef:S -> S
        end,
    case Stk of
        % AVM_CREATE_STACKTRACES=off: no stacktrace to inspect, the undef
        % itself (caught above) is all this build can assert.
        undefined ->
            ok;
        [{atomvm_no_such_module, no_such_fun, A, _Loc} | _] ->
            true = (A =:= 3 orelse A =:= [1, 2, 3]),
            ok
    end.

test_md5() ->
    Digest = erlang:md5(<<"hello">>),
    16 = byte_size(Digest),
    % md5 accepts iodata, as used by beam_asm on code chunks
    Digest = erlang:md5([<<"he">>, [<<"l">>, $l], <<"o">>]),
    Empty = erlang:md5(<<>>),
    Empty = erlang:md5([]),
    ok.

test_standard_error() ->
    ok = io:put_chars(standard_error, "(stderr smoke) "),
    ok = io:format(standard_error, "~p ", [stderr_format_smoke]),
    ok.
