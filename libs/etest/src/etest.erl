%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Fred Dushin <fred@dushin.net>
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
%% @doc This modules provides a basic testing framework for AtomVM Erlang
%% libraries.
%% @end
%%-----------------------------------------------------------------------------
-module(etest).

-export([test/1]).
-export([assert_match/2, assert_equals/2, assert_true/1, assert_failure/1, assert_failure/2]).

%%-----------------------------------------------------------------------------
%% @param   Tests a list of test modules
%% @returns ok if all of the tests pass, or the atom fail, if any of the tests
%%          failed.
%% @doc     Test a sequence of test modules.
%%
%%          This function will execute the test/0 function for each module
%%          provided in the input list of test modules.  If all of the tests
%%          return the atom ok, then this function returns ok.  If any of the
%%          test modules return a value other than ok, then this function
%%          returns the atom fail.
%% @end
%%-----------------------------------------------------------------------------
-spec test(list(module())) -> ok | fail.
test(Tests) ->
    Results = [{Test, run_test(Test)} || Test <- Tests],
    case erlang:system_info(machine) of
        "BEAM" -> io:format("\n");
        _ -> console:puts("\n")
    end,
    erlang:display(Results),
    check_results(Results).

%%-----------------------------------------------------------------------------
%% @param   X a term
%% @param   Y a term
%% @returns ok if X and Y unify; fail otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec assert_match(term(), term()) -> ok | fail.
assert_match(X, X) -> ok;
assert_match(_, _) -> fail.

%%-----------------------------------------------------------------------------
%% @param   X a term
%% @param   Y a term
%% @returns ok if X and Y are equal; fail otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec assert_equals(term(), term()) -> ok | fail.
assert_equals(X, Y) ->
    case X == Y of
        true -> ok;
        _ -> fail
    end.

%%-----------------------------------------------------------------------------
%% @param   X a term
%% @returns ok if X is true; fail otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec assert_true(boolean()) -> ok | fail.
assert_true(true) -> ok;
assert_true(_) -> fail.

%%-----------------------------------------------------------------------------
%% @param   F a function to evaluate
%% @returns ok if evaluating F results in Error being thrown; fail, otherwise
%% @end
%%-----------------------------------------------------------------------------
-spec assert_failure(fun()) -> ok | fail.
assert_failure(F) ->
    try
        F(),
        fail
    catch
        _:_ -> ok
    end.

%%-----------------------------------------------------------------------------
%% @param   F a function to evaluate
%% @returns ok if evaluating F results in Error being thrown; fail, otherwise
%% @end
%%-----------------------------------------------------------------------------
-spec assert_failure(fun(), Error :: atom()) -> ok | fail.
assert_failure(F, E) ->
    try
        F(),
        fail
    catch
        %% TODO implement opcode 108 (raise/2)
        _:E ->
            id(E),
            ok
    end.

%%=============================================================================
%% internal operations

%% @private
run_test(Test) ->
    Parent = self(),
    {Pid, Ref} = spawn_opt(
        fun() ->
            Result = do_run_test(Test),
            Parent ! {self(), Result}
        end,
        [monitor]
    ),
    receive
        {Pid, Result} ->
            receive
                {'DOWN', Ref, process, Pid, normal} -> ok
            after 0 -> ok
            end,
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    end.

do_run_test(Test) ->
    try
        Result = Test:test(),
        Value = process_flag(trap_exit, false),
        case Value of
            true ->
                erlang:display({test, Test, unexpected_trap_exit});
            false ->
                ok
        end,
        case erlang:system_info(machine) of
            "BEAM" ->
                io:format("+");
            _ ->
                console:puts("+"),
                console:flush()
        end,
        receive
            Garbage -> erlang:display({test, Test, unexpected_msg, Garbage})
        after 0 -> ok
        end,
        Result
    catch
        _:E:S ->
            case erlang:system_info(machine) of
                "BEAM" ->
                    io:format("-");
                _ ->
                    console:puts("-"),
                    console:flush()
            end,
            {exception, {E, S}}
    end.

%% @private
check_results([]) ->
    ok;
check_results([{_Test, ok} | T]) ->
    check_results(T);
check_results([Failure | _T]) ->
    {fail, Failure}.

id(X) -> X.
