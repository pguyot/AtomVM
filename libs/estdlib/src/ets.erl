%
% This file is part of AtomVM.
%
% Copyright 2024 Fred Dushin <fred@dushin.net>
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
%% @doc A limited implementation of the Erlang/OTP `ets' module.
%% @end
%%-----------------------------------------------------------------------------
-module(ets).

-export([
    new/2,
    lookup/2,
    lookup_element/3,
    lookup_element/4,
    member/2,
    insert/2,
    insert_new/2,
    update_element/3,
    update_element/4,
    update_counter/3,
    update_counter/4,
    take/2,
    delete/1,
    delete/2,
    delete_object/2,
    tab2list/1,
    select/2,
    select/3,
    select_count/2,
    select_delete/2,
    match_delete/2,
    info/2
]).

-export_type([
    table/0,
    options/0,
    table_type/0,
    access_type/0,
    update_op/0
]).

-opaque table() :: atom | reference().
-type table_type() :: set | bag | duplicate_bag.
-type access_type() :: private | protected | public.
-type option() :: table_type() | {keypos, non_neg_integer()} | access_type().
-type options() :: [option()].
-type update_op() ::
    {pos_integer(), integer()} | {pos_integer(), integer(), integer(), integer()}.

%%-----------------------------------------------------------------------------
%% @param   Name the ets table name
%% @param   Options the options used to create the table
%% @returns A new ets table
%% @doc Create a new ets table.
%%
%% Supported table types are `set', `bag', and `duplicate_bag'.
%% The `ordered_set' type is not currently supported.
%% @end
%%-----------------------------------------------------------------------------
-spec new(Name :: atom(), Options :: options()) -> table().
new(_Name, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries
%% @returns a list of matching tuples, or an empty list if none found
%% @doc Look up an entry in an ets table.
%%
%% For `set' tables, returns at most one element. For `bag' and `duplicate_bag'
%% tables, returns all objects with the matching key.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup(Table :: table(), Key :: term()) -> [tuple()].
lookup(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries
%% @param   Pos index of the element to retrieve (1-based)
%% @returns the element at position Pos from the matching tuple, or a list of
%%          such elements if the table is of type `bag' or `duplicate_bag'
%% @doc Look up an element from an entry in an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_element(Table :: table(), Key :: term(), Pos :: pos_integer()) -> term() | [term()].
lookup_element(_Table, _Key, _Pos) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries
%% @param   Pos index of the element to retrieve (1-based)
%% @param   Default value returned if the key does not exist
%% @returns the element at position Pos from the matching tuple, or a list of
%%          such elements if the table is of type `bag' or `duplicate_bag',
%%          or Default if the key does not exist
%% @doc Look up an element from an entry in an ets table with a default value.
%%
%% Unlike `lookup_element/3', returns Default instead of raising `badarg' when
%% the key does not exist.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_element(Table :: table(), Key :: term(), Pos :: pos_integer(), Default :: term()) ->
    term() | [term()].
lookup_element(_Table, _Key, _Pos, _Default) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key to check for existence
%% @returns true if the key exists in the table; false otherwise
%% @doc Check if a key exists in an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec member(Table :: table(), Key :: term()) -> boolean().
member(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Entry the entry or list of entries to insert
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Insert an entry into an ets table.
%%
%% For `set' tables, an existing entry with the same key is overwritten.
%% For `bag' tables, the object is added unless an identical object already
%% exists. For `duplicate_bag' tables, the object is always added.
%% The operation is atomic.
%% @end
%%-----------------------------------------------------------------------------

-spec insert(Table :: table(), Entry :: tuple() | [tuple()]) -> true.
insert(_Table, _Entry) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Entry the entry or list of entries to insert
%% @returns true if all entries were inserted; false if any key already exists
%% @doc Insert an entry into an ets table only if the key does not already exist.
%% @end
%%-----------------------------------------------------------------------------
-spec insert_new(Table :: table(), Entry :: tuple() | [tuple()]) -> boolean().
insert_new(_Table, _Entry) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry to update
%% @param   ElementSpec a tuple {Pos, Value} or a list of such tuples, specifying
%%          the position(s) (1-based) and new value(s) to set
%% @returns true if the entry was updated; false if the key does not exist
%% @doc Update one or more elements of an existing entry in an ets table.
%%
%% The key field itself cannot be updated. Returns `false' if no entry with
%% the given key exists.
%% @end
%%-----------------------------------------------------------------------------
-spec update_element(
    Table :: table(),
    Key :: term(),
    ElementSpec :: {pos_integer(), term()} | [{pos_integer(), term()}]
) -> boolean().
update_element(_Table, _Key, _ElementSpec) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry to update
%% @param   ElementSpec a tuple {Pos, Value} or a list of such tuples, specifying
%%          the position(s) (1-based) and new value(s) to set
%% @param   Default a default tuple to insert if the key does not exist
%% @returns true if the entry was updated or inserted; false if insertion failed
%% @doc Update one or more elements of an existing entry, inserting Default if missing.
%%
%% If no entry with the given key exists, inserts Default into the table,
%% then applies the element updates.
%% @end
%%-----------------------------------------------------------------------------
-spec update_element(
    Table :: table(),
    Key :: term(),
    ElementSpec :: {pos_integer(), term()} | [{pos_integer(), term()}],
    Default :: tuple()
) -> boolean().
update_element(_Table, _Key, _ElementSpec, _Default) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry expecting to contain a tuple
%%          of integers or a single integer
%% @param   Params an integer increment, a single update operation, or a list
%%          of update operations. An update operation is a tuple
%%          `{Pos, Increment}' or `{Pos, Increment, Threshold, SetValue}',
%%          where Pos is a 1-based index.
%% @returns the new counter value, or a list of new values when Params is a list
%% @doc Updates one or more counter values at Key in the table.
%% @end
%%-----------------------------------------------------------------------------
-spec update_counter(
    Table :: table(),
    Key :: term(),
    Params :: integer() | update_op() | [update_op()]
) -> integer() | [integer()].
update_counter(_Table, _Key, _Params) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry expecting to contain a tuple
%%         of integers or a single integer
%% @param   Params an integer increment, a single update operation, or a list
%%          of update operations (see `update_counter/3' for the format)
%% @param   Default a default object (tuple) to insert if the key does not
%%          exist, after which the update operation is applied to it
%% @returns the new counter value, or a list of new values when Params is a list
%% @doc Updates one or more counter values at Key in the table.
%%
%% Equivalent to `update_counter/3', but inserts Default as a new entry if
%% no object with Key exists, then performs the counter update on it.
%% @end
%%-----------------------------------------------------------------------------
-spec update_counter(
    Table :: table(),
    Key :: term(),
    Params :: integer() | update_op() | [update_op()],
    Default :: tuple()
) -> integer() | [integer()].
update_counter(_Table, _Key, _Params, _Default) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up and remove entries
%% @returns a list of the removed objects, or an empty list if none found
%% @doc Return and delete all entries with the given key from an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec take(Table :: table(), Key :: term()) -> [tuple()].
take(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @returns true;
%% @doc Delete an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Table :: table()) -> true.
delete(_Table) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries to delete
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Delete all entries with the given key from an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Table :: table(), Key :: term()) -> true.
delete(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Object the exact object to delete
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Delete a specific object from an ets table.
%%
%% Unlike `delete/2', which deletes all entries matching a key, this function
%% deletes only entries that exactly match the given object. For `bag' tables,
%% other objects sharing the same key are left intact. For `duplicate_bag'
%% tables, all instances of the identical object are removed.
%% @end
%%-----------------------------------------------------------------------------
-spec delete_object(Table :: table(), Object :: tuple()) -> true.
delete_object(_Table, _Object) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @returns a list of all objects in the table
%% @doc Return all objects of an ets table. The order is unspecified.
%% @end
%%-----------------------------------------------------------------------------
-spec tab2list(Table :: table()) -> [tuple()].
tab2list(_Table) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   MatchSpec a match specification
%% @returns the list of results from applying the match spec to the objects
%% @doc Match objects of a table against a match specification.
%%
%% This is implemented as a full table traversal with the match specification
%% interpreted in Erlang: a commonly used subset of the match specification
%% language is supported (patterns with `'$N''/`'_'', guard tests built from
%% guard BIFs including `andalso'/`orelse', and bodies built from bound
%% variables, `'$_'', `'$$'', `{const, T}', tuple construction and guard BIF
%% calls). The result order is unspecified, as for `set' tables on
%% Erlang/OTP.
%% @end
%%-----------------------------------------------------------------------------
-spec select(Table :: table(), MatchSpec :: [{term(), [term()], [term()]}]) -> [term()].
select(Table, MatchSpec) when is_list(MatchSpec) ->
    % fully qualified so the call resolves to the NIF, not this stub module
    Objects = ?MODULE:tab2list(Table),
    select_objects(Objects, MatchSpec, []).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   MatchSpec a match specification
%% @param   Limit maximum number of results to return
%% @returns `{Matches, Continuation}' or `'$end_of_table''
%% @doc Like `select/2' but returns at most `Limit' results.
%%
%% The whole table is traversed at once: the returned continuation is always
%% `'$end_of_table'' and only the first `Limit' matches are returned.
%% @end
%%-----------------------------------------------------------------------------
-spec select(Table :: table(), MatchSpec :: [{term(), [term()], [term()]}], Limit :: pos_integer()) ->
    {[term()], term()} | '$end_of_table'.
select(Table, MatchSpec, Limit) when is_integer(Limit), Limit > 0 ->
    case select(Table, MatchSpec) of
        [] -> '$end_of_table';
        Results -> {lists_sublist(Results, Limit), '$end_of_table'}
    end.

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   MatchSpec a match specification
%% @returns the number of objects matched by the match specification
%% @doc Count the objects of a table matching a match specification.
%% @end
%%-----------------------------------------------------------------------------
-spec select_count(Table :: table(), MatchSpec :: [{term(), [term()], [term()]}]) ->
    non_neg_integer().
select_count(Table, MatchSpec) ->
    length(select(Table, MatchSpec)).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   MatchSpec a match specification
%% @returns the number of objects deleted
%% @doc Delete the objects of a table for which the match specification
%% returns `true'.
%% @end
%%-----------------------------------------------------------------------------
-spec select_delete(Table :: table(), MatchSpec :: [{term(), [term()], [term()]}]) ->
    non_neg_integer().
select_delete(Table, MatchSpec) when is_list(MatchSpec) ->
    % fully qualified so the call resolves to the NIF, not this stub module
    Objects = ?MODULE:tab2list(Table),
    select_delete0(Objects, Table, MatchSpec, 0).

%% @private
select_delete0([], _Table, _MatchSpec, Count) ->
    Count;
select_delete0([Object | Tail], Table, MatchSpec, Count) ->
    case run_match_spec(MatchSpec, Object) of
        {ok, true} ->
            ?MODULE:delete_object(Table, Object),
            select_delete0(Tail, Table, MatchSpec, Count + 1);
        _ ->
            select_delete0(Tail, Table, MatchSpec, Count)
    end.

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Pattern a match pattern, as used in `match/2'
%% @returns `true'
%% @doc Delete all objects of a table matching a pattern.
%% @end
%%-----------------------------------------------------------------------------
-spec match_delete(Table :: table(), Pattern :: term()) -> true.
match_delete(Table, Pattern) ->
    _ = select_delete(Table, [{Pattern, [], [true]}]),
    true.

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Item the information item to query
%% @returns the value of the item, or `undefined' for unsupported items
%% @doc Return information about a table.
%%
%% Only `size' is currently supported (computed by a full table traversal);
%% any other item returns `undefined'.
%% @end
%%-----------------------------------------------------------------------------
-spec info(Table :: table(), Item :: atom()) -> term() | undefined.
info(Table, size) ->
    length(?MODULE:tab2list(Table));
info(_Table, _Item) ->
    undefined.

%% @private
select_objects([], _MatchSpec, Acc) ->
    lists:reverse(Acc);
select_objects([Object | Tail], MatchSpec, Acc) ->
    case run_match_spec(MatchSpec, Object) of
        {ok, Result} -> select_objects(Tail, MatchSpec, [Result | Acc]);
        nomatch -> select_objects(Tail, MatchSpec, Acc)
    end.

%% @private
run_match_spec([], _Object) ->
    nomatch;
run_match_spec([{Head, Guards, Body} | Tail], Object) ->
    case ms_match(Head, Object, #{}) of
        {ok, Bindings0} ->
            Bindings = Bindings0#{'$_' => Object},
            case ms_guards(Guards, Bindings) of
                true -> {ok, ms_body(Body, Bindings)};
                false -> run_match_spec(Tail, Object)
            end;
        nomatch ->
            run_match_spec(Tail, Object)
    end.

%% @private
%% Pattern matching: patterns are literals, '_', '$N' variables, and
%% tuples/lists thereof.
ms_match('_', _Value, Bindings) ->
    {ok, Bindings};
ms_match(Pattern, Value, Bindings) when is_atom(Pattern) ->
    case ms_variable(Pattern) of
        {ok, Var} ->
            case Bindings of
                #{Var := Bound} ->
                    case Bound =:= Value of
                        true -> {ok, Bindings};
                        false -> nomatch
                    end;
                _ ->
                    {ok, Bindings#{Var => Value}}
            end;
        not_a_variable ->
            case Pattern =:= Value of
                true -> {ok, Bindings};
                false -> nomatch
            end
    end;
ms_match(Pattern, Value, Bindings) when is_tuple(Pattern) ->
    case is_tuple(Value) andalso tuple_size(Pattern) =:= tuple_size(Value) of
        true -> ms_match_tuple(Pattern, Value, tuple_size(Pattern), Bindings);
        false -> nomatch
    end;
ms_match([PH | PT], Value, Bindings) ->
    case Value of
        [VH | VT] ->
            case ms_match(PH, VH, Bindings) of
                {ok, NewBindings} -> ms_match(PT, VT, NewBindings);
                nomatch -> nomatch
            end;
        _ ->
            nomatch
    end;
ms_match(Pattern, Value, Bindings) ->
    case Pattern =:= Value of
        true -> {ok, Bindings};
        false -> nomatch
    end.

%% @private
ms_match_tuple(_Pattern, _Value, 0, Bindings) ->
    {ok, Bindings};
ms_match_tuple(Pattern, Value, N, Bindings) ->
    case ms_match(element(N, Pattern), element(N, Value), Bindings) of
        {ok, NewBindings} -> ms_match_tuple(Pattern, Value, N - 1, NewBindings);
        nomatch -> nomatch
    end.

%% @private
ms_variable(Atom) ->
    case atom_to_list(Atom) of
        [$$ | Digits] when Digits =/= [] ->
            case ms_all_digits(Digits) of
                true -> {ok, Atom};
                false -> not_a_variable
            end;
        _ ->
            not_a_variable
    end.

%% @private
ms_all_digits([]) -> true;
ms_all_digits([C | T]) when C >= $0, C =< $9 -> ms_all_digits(T);
ms_all_digits(_) -> false.

%% @private
ms_guards([], _Bindings) ->
    true;
ms_guards([Guard | Tail], Bindings) ->
    Result =
        try
            ms_expr(Guard, Bindings)
        catch
            _:_ -> false
        end,
    case Result of
        true -> ms_guards(Tail, Bindings);
        _ -> false
    end.

%% @private
ms_body(Body, Bindings) ->
    lists:foldl(fun(Expr, _) -> ms_expr(Expr, Bindings) end, [], Body).

%% @private
%% Expression evaluation, shared between guards and bodies.
ms_expr('$_', #{'$_' := Object}) ->
    Object;
ms_expr('$$', Bindings) ->
    Vars = lists:sort(maps:keys(maps:remove('$_', Bindings))),
    [maps:get(V, Bindings) || V <- Vars];
ms_expr(Atom, Bindings) when is_atom(Atom) ->
    case ms_variable(Atom) of
        {ok, Var} ->
            case Bindings of
                #{Var := Value} -> Value;
                _ -> error({unbound_match_spec_variable, Var})
            end;
        not_a_variable ->
            Atom
    end;
ms_expr({const, Term}, _Bindings) ->
    Term;
ms_expr({{}}, _Bindings) ->
    {};
ms_expr(Tuple, Bindings) when
    is_tuple(Tuple), tuple_size(Tuple) =:= 1, is_tuple(element(1, Tuple))
->
    % {{...}} is the tuple construction syntax
    Elements = tuple_to_list(element(1, Tuple)),
    list_to_tuple([ms_expr(E, Bindings) || E <- Elements]);
ms_expr({'andalso', A, B}, Bindings) ->
    case ms_expr(A, Bindings) of
        true -> ms_expr(B, Bindings);
        _ -> false
    end;
ms_expr({'orelse', A, B}, Bindings) ->
    case ms_expr(A, Bindings) of
        true -> true;
        _ -> ms_expr(B, Bindings)
    end;
ms_expr(Tuple, Bindings) when is_tuple(Tuple), tuple_size(Tuple) >= 1, is_atom(element(1, Tuple)) ->
    [Fun | Args] = tuple_to_list(Tuple),
    EvaledArgs = [ms_expr(A, Bindings) || A <- Args],
    apply(erlang, Fun, EvaledArgs);
ms_expr(List, Bindings) when is_list(List) ->
    [ms_expr(E, Bindings) || E <- List];
ms_expr(Literal, _Bindings) ->
    Literal.

%% @private
%% lists:sublist/2 is not part of AtomVM's lists module on all versions;
%% keep a local copy.
lists_sublist(List, Len) when Len >= length(List) -> List;
lists_sublist(_List, 0) -> [];
lists_sublist([H | T], Len) -> [H | lists_sublist(T, Len - 1)].
