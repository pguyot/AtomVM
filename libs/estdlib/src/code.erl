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
%% @doc An implementation of a subset of the Erlang/OTP code interface.
%% @end
%%-----------------------------------------------------------------------------
-module(code).

-export([
    all_available/0,
    all_loaded/0,
    load_abs/1,
    load_binary/3,
    ensure_loaded/1,
    ensure_modules_loaded/1,
    which/1,
    where_is_file/1,
    is_loaded/1,
    get_object_code/1,
    purge/1,
    delete/1,
    add_patha/1,
    add_pathz/1,
    del_path/1,
    lib_dir/1
]).

%%-----------------------------------------------------------------------------
%% @param   Module      module to purge
%% @returns `false'
%% @doc     Compatibility stub. AtomVM has no notion of old code: loading a
%% module that is already loaded makes the new version current for name
%% lookups and the previous version is simply never reclaimed, so there is
%% never old code to purge (and no process is ever killed).
%% @end
%%-----------------------------------------------------------------------------
-spec purge(Module :: module()) -> boolean().
purge(_Module) ->
    false.

%%-----------------------------------------------------------------------------
%% @param   Module      module to delete
%% @returns `true'
%% @doc     Compatibility stub, see {@link purge/1}. The module keeps
%% resolving by name until a new version is loaded over it.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Module :: module()) -> boolean().
delete(_Module) ->
    true.

%%-----------------------------------------------------------------------------
%% @param   Dir directory to add to the code path
%% @returns `true'
%% @doc     Compatibility stub. AtomVM loads modules from avm packs rather
%% than from a directory search path, so the path is ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec add_patha(Dir :: string() | binary()) -> true.
add_patha(_Dir) ->
    true.

%%-----------------------------------------------------------------------------
%% @param   Dir directory to add to the code path
%% @returns `true'
%% @doc     Compatibility stub, see {@link add_patha/1}.
%% @end
%%-----------------------------------------------------------------------------
-spec add_pathz(Dir :: string() | binary()) -> true.
add_pathz(_Dir) ->
    true.

%%-----------------------------------------------------------------------------
%% @param   NameOrDir directory or application name to remove from the code path
%% @returns `false'
%% @doc     Compatibility stub, see {@link add_patha/1}. AtomVM has no code
%% path, so there is never a directory to delete and `false' is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec del_path(NameOrDir :: atom() | string() | binary()) -> boolean() | {error, bad_name}.
del_path(_NameOrDir) ->
    false.

%%-----------------------------------------------------------------------------
%% @param   AppName name of the application
%% @returns the library directory of the application, or `{error, bad_name}'
%% @doc     Return the library directory of an application inside an
%% Erlang/OTP installation designated by the `ATOMVM_OTP_LIB_DIR'
%% environment variable (AtomVM has no code path of its own). This makes
%% compile-time include resolution (e.g. `-include_lib' and record
%% extraction) work like on Erlang/OTP when an OTP installation is
%% available.
%% @end
%%-----------------------------------------------------------------------------
-spec lib_dir(AppName :: atom()) -> string() | {error, bad_name}.
lib_dir(AppName) when is_atom(AppName) ->
    case os:getenv("ATOMVM_OTP_LIB_DIR") of
        false ->
            {error, bad_name};
        Root ->
            Prefix = atom_to_list(AppName) ++ "-",
            case file:list_dir(Root) of
                {ok, Entries} ->
                    Matching = [
                        E
                     || E <- Entries,
                        lists:prefix(Prefix, entry_to_list(E)) orelse
                            entry_to_list(E) =:= atom_to_list(AppName)
                    ],
                    case lists:sort(Matching) of
                        [] -> {error, bad_name};
                        Sorted -> Root ++ "/" ++ entry_to_list(lists:last(Sorted))
                    end;
                {error, _} ->
                    {error, bad_name}
            end
    end.

%% @private
entry_to_list(E) when is_binary(E) -> binary_to_list(E);
entry_to_list(E) when is_list(E) -> E.

%%-----------------------------------------------------------------------------
%% @returns A list of available modules, including loaded modules
%% @doc     Return all modules available from loaded avm packs, in addition
%%          to loaded modules. List of available modules may be incomplete if
%%          this function is called while a module is loaded.
%%          As on Erlang/OTP, module names are strings; the second element of
%%          each tuple is currently unspecified (`undefined').
%% @end
%%-----------------------------------------------------------------------------
-spec all_available() -> [{string(), term(), boolean()}].
all_available() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A list of all loaded modules
%% @doc     Return a list of all loaded modules.
%%          Result type differs from Erlang/OTP: second term of tuples is
%%          currently unspecified
%% @end
%%-----------------------------------------------------------------------------
-spec all_loaded() -> [{atom(), term()}].
all_loaded() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Filename    path to the beam to open, without .beam suffix
%% @returns A tuple with the name of the module
%% @doc     Load a module from a path.
%% Error return result type is different from Erlang/OTP.
%% @end
%%-----------------------------------------------------------------------------
-spec load_abs(Filename :: string()) -> error | {module, module()}.
load_abs(_Filename) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      name of the module to load
%% @param   Filename    path to the beam (unused)
%% @param   Binary      binary of the module to load
%% @returns A tuple with the name of the module
%% @doc     Load a module from a binary.
%% Error return result type is different from Erlang/OTP.
%% Also unlike Erlang/OTP, no check is performed to verify that `Module'
%% matches the name of the loaded module.
%% @end
%%-----------------------------------------------------------------------------
-spec load_binary(Module :: module(), Filename :: string(), Binary :: binary()) ->
    error | {module, module()}.
load_binary(_Module, _Filename, _Binary) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      module to load
%% @returns Tuple `{module, Module}' if module is loaded or `{error, embedded}'
%% @doc     Try to load a module if it's not already loaded. AtomVM works in
%% an embedded-like mode where modules are loaded at start-up but modules
%% can be loaded explicitely as well (especially from a binary with `load_binary/3').
%% So this function can be used to determine if a module is loaded.
%% It is called by Elixir Code module.
%% @end
%%-----------------------------------------------------------------------------
-spec ensure_loaded(Module) -> {module, Module} | {error, embedded | any()} when
    Module :: atom().
ensure_loaded(_Module) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Modules     modules to load
%% @returns `ok', or `{error, [{Module, Reason}]}' listing the modules that
%%          could not be loaded
%% @doc     Try to load a list of modules, see {@link ensure_loaded/1}.
%% @end
%%-----------------------------------------------------------------------------
-spec ensure_modules_loaded(Modules :: [module()]) -> ok | {error, [{module(), term()}]}.
ensure_modules_loaded(Modules) when is_list(Modules) ->
    Errors = lists:foldl(
        fun(Module, Acc) ->
            case ?MODULE:ensure_loaded(Module) of
                {module, Module} -> Acc;
                {error, Reason} -> [{Module, Reason} | Acc]
            end
        end,
        [],
        Modules
    ),
    case Errors of
        [] -> ok;
        _ -> {error, lists:reverse(Errors)}
    end.

%%-----------------------------------------------------------------------------
%% @param   Filename    name of the file to search for
%% @returns `non_existing'
%% @doc     Compatibility stub, see {@link add_patha/1}. AtomVM has no code
%% path to search, so this always returns `non_existing'.
%% @end
%%-----------------------------------------------------------------------------
-spec where_is_file(Filename :: string()) -> non_existing | string().
where_is_file(_Filename) ->
    non_existing.

%%-----------------------------------------------------------------------------
%% @param   Module      module to test
%% @returns Tuple `{file, preloaded}' if module is loaded or `false'
%% @doc     Determine if a module is loaded. AtomVM works in
%% an embedded-like mode where modules are loaded at start-up but modules
%% can be loaded explicitely as well (especially from a binary with `load_binary/3').
%% @end
%%-----------------------------------------------------------------------------
is_loaded(Module) ->
    case ?MODULE:ensure_loaded(Module) of
        {module, _Module} ->
            {file, preloaded};
        {error, _} ->
            false
    end.

%%-----------------------------------------------------------------------------
%% @param   Module      module to test
%% @returns `preloaded' if module is loaded or `false'
%% @doc     Determine if a module is loaded. There currently is no way to
%% distinguish a module that was loaded with `load_binary/3' or that was
%% preloaded at startup.
%% @end
%%-----------------------------------------------------------------------------
which(Module) ->
    case ?MODULE:ensure_loaded(Module) of
        {module, _Module} ->
            preloaded;
        {error, _} ->
            non_existing
    end.

%%-----------------------------------------------------------------------------
%% @param   Module      module to get object code from
%% @returns Tuple `{Module, Binary, Filename}' if successful, otherwise `error'.
%% @doc     Return module binary of a given module. Note: this function doesn't
%% behave like on the BEAM: on the BEAM, modules loaded from a binary with
%% `code:load_binary/3' are not returned, while On AtomVM they are returned.
%% This behavior might change in the future.
%% @end
%%-----------------------------------------------------------------------------
-spec get_object_code(Module) -> {Module, Binary, Filename} | error when
    Module :: atom(), Binary :: binary(), Filename :: string().
get_object_code(_Module) ->
    erlang:nif_error(undefined).
