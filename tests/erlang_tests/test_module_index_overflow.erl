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

%% Regression test for the >256-module continuation-pointer overflow.
%%
%% A return address (continuation pointer) encodes the module to resume into.
%% On 32-bit platforms the historical packing reserved only 8 bits for the module
%% index, so once more than 256 modules were loaded a process returning into a
%% module with index >= 256 jumped into the wrong module and crashed. The cp now
%% stores the module by pointer (32-bit) / wide index (64-bit), lifting the cap.
%%
%% This test loads 300 distinct modules at runtime so module indices climb well
%% past 256, then runs a body-recursive function in each. export_test_module's
%% `exported_func/1' does `?MODULE:exported_func(N - 1) * N', i.e. non-tail
%% recursive calls whose saved continuation pointer carries that module's
%% (possibly >= 256) index. Returning through those frames is exactly what used
%% to corrupt on 32-bit.
%%
%% The 300 modules are made by patching the 18-character module-name atom of a
%% single embedded base module (export_test_module) in place to 300 distinct
%% same-length names, so no extra build artifacts are needed.

-module(test_module_index_overflow).

-export([start/0]).

-include("code_load/export_test_module_data.hrl").

-define(MODULE_COUNT, 300).
-define(BASE_NAME, <<"export_test_module">>).

start() ->
    Base = ?EXPORT_TEST_MODULE_DATA,
    ok = load_all(Base, 1),
    %% exported_func(4) = 4! = 24 for every loaded module; summing the results
    %% (24 * 300 = 7200) proves each high-index module returned correctly -- a
    %% corrupted cp would crash or jump into the wrong module rather than
    %% return 24.
    call_all(1, 0).

%% 18-char name matching the length of "export_test_module":
%% "export_test_md_" (15 chars) ++ 3-digit zero-padded index (1..999).
mod_name(I) when I < 10 ->
    list_to_atom("export_test_md_00" ++ integer_to_list(I));
mod_name(I) when I < 100 ->
    list_to_atom("export_test_md_0" ++ integer_to_list(I));
mod_name(I) ->
    list_to_atom("export_test_md_" ++ integer_to_list(I)).

load_all(_Base, I) when I > ?MODULE_COUNT ->
    ok;
load_all(Base, I) ->
    Name = mod_name(I),
    NameBin = atom_to_binary(Name, latin1),
    Bin = binary:replace(Base, ?BASE_NAME, NameBin, [global]),
    {module, Name} = code:load_binary(Name, atom_to_list(Name) ++ ".beam", Bin),
    load_all(Base, I + 1).

call_all(I, Acc) when I > ?MODULE_COUNT ->
    Acc;
call_all(I, Acc) ->
    Name = mod_name(I),
    24 = Name:exported_func(4),
    call_all(I + 1, Acc + 24).
