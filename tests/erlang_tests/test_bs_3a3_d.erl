%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
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
-module(test_bs_3a3_d).

-export([start/0]).

start() ->
    ok = expect_error(fun() -> skip_bits(128, <<"foobar">>) end, {badmatch, <<"foobar">>}),
    0.

skip_bits(Len, Bin) ->
    <<_First:Len, Rest/binary>> = Bin,
    Rest.

expect_error(F, Reason) when is_atom(Reason) orelse is_tuple(Reason) ->
    expect_error(F, fun(Tag, Value) -> Tag =:= error andalso Value =:= Reason end);
expect_error(F, ErrorValidator) when is_function(ErrorValidator) ->
    ok =
        try
            F(),
            unexpected
        catch
            T:V ->
                case ErrorValidator(T, V) of
                    false ->
                        erlang:display({T, V}),
                        {got, {T, V}, validator_failed};
                    true ->
                        ok
                end
        end.
