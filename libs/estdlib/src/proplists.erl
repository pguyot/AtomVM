%
% This file is part of AtomVM.
%
% Copyright 2018-2023 Fred Dushin <fred@dushin.net>
% Copyright 2000-2003 Richard Carlsson <carlsson.richard@gmail.com>
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
%% @doc An implementation of the Erlang/OTP proplists interface.
%%
%% This module implements a strict subset of the Erlang/OTP proplists
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(proplists).

-export([
    property/1, property/2,
    compact/1,
    substitute_aliases/2,
    substitute_negations/2,
    expand/2,
    normalize/2,
    unfold/1,
    delete/2,
    get_bool/2,
    get_all_values/2,
    get_value/2, get_value/3,
    is_defined/2,
    lookup/2,
    lookup_all/2,
    from_map/1,
    to_map/1
]).

-export_type([property/0, proplist/0]).

-type property() :: atom() | {term(), term()}.
-type proplist() :: [property()].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   PropertyIn a property
%% @returns the same property in normal form
%% @doc     Creates a normal form (minimal) representation of a property. If `PropertyIn' is
%%          `{Key, true}', where `Key' is an atom, `Key' is returned, otherwise the whole
%%          term `PropertyIn' is returned.
%%          See also `property/2'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec property(PropertyIn) -> PropertyOut when
    PropertyIn :: property(),
    PropertyOut :: property().

property({Key, true}) when is_atom(Key) ->
    Key;
property(Property) ->
    Property.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the property key
%% @param   Value the property value
%% @returns Creates a property in normal form
%% @doc     Creates a normal form (minimal) representation of a simple key/value property.
%%          Returns `Key' if `Value' is `true' and `Key' is an atom, otherwise a tuple
%%          `{Key, Value}' is returned.
%%          See also `property/1'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec property(Key, Value) -> Property when
    Key :: term(),
    Value :: term(),
    Property :: atom() | {term(), term()}.

property(Key, true) when is_atom(Key) ->
    Key;
property(Key, Value) ->
    {Key, Value}.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the item key that will be deleted
%% @param   List the property list from which items will be deleted
%% @returns A list without items having key `Key'
%% @doc     Deletes all entries associated with `Key' from `List'.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Key, List) -> List when
    Key :: term(),
    List :: [term()].
delete(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            delete(Key, Ps);
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            delete(Key, Ps);
        true ->
            [P | delete(Key, Ps)]
    end;
delete(_, []) ->
    [].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key that will be searched
%% @param   List the list where key is searched
%% @returns `true' if `Key' is defined, otherwise false
%% @doc     Returns `true' if `List' contains at least one entry associated with `Key', otherwise
%%          `false'.
%% @end
%%-----------------------------------------------------------------------------
-spec is_defined(Key, List) -> boolean() when
    Key :: term(),
    List :: [term()].

is_defined(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            true;
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            true;
        true ->
            is_defined(Key, Ps)
    end;
is_defined(_Key, []) ->
    false.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key that will be searched
%% @param   List the list where key is searched
%% @returns `true' when exists an option with given key that is `true', otherwise `false'
%% @doc     Returns the value of a boolean key/value option. If
%%          [`lookup(Key, List)'](`lookup/2') would yield `{Key, true}', this function
%%          returns `true', otherwise `false'.
%%          See also `get_value/2', `lookup/2'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_bool(Key, List) -> boolean() when
    Key :: term(),
    List :: [term()].

get_bool(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            true;
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            case P of
                {_, true} ->
                    true;
                _ ->
                    %% Don't continue the search!
                    false
            end;
        true ->
            get_bool(Key, Ps)
    end;
get_bool(_Key, []) ->
    false.

%%-----------------------------------------------------------------------------
%% @equiv   get_value(Key, List, undefined)
%% @doc     Get a value from a property list.
%% @end
%%-----------------------------------------------------------------------------
-spec get_value(Key :: term(), List :: list(property())) -> term() | true | undefined.
get_value(Key, List) ->
    get_value(Key, List, undefined).

%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the value
%% @param   List the property list from which to get the value
%% @param   Default the default value to return, if Key is not in the property list.
%% @returns the value in the property list under the key, or Default, if Key is
%%          not in List.
%% @doc     Get a value from a property list.
%%
%%          Returns the value under the specified key, or the specified Default,
%%          if the Key is not in the supplied List.  If the Key corresponds to
%%          an entry in the property list that is just a single atom, this
%%          function returns the atom true.
%% @end
%%-----------------------------------------------------------------------------
-spec get_value(Key :: term(), List :: list(property()), Default :: term()) -> term().
get_value(_Key, [], Default) ->
    Default;
get_value(Key, [{Key, Value} | _T], _Default) ->
    Value;
get_value(Key, [Key | _T], _Default) when is_atom(Key) ->
    true;
get_value(Key, [_H | T], Default) ->
    get_value(Key, T, Default).

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the values
%% @param   List the property list from which to get the value
%% @returns a list of values for all entries having `Key' as key
%% @doc     Similar to `get_value/2', but returns the list of values for _all_ entries
%%          `{Key, Value}' in `List'. If no such entry exists, the result is the empty list.
%% @end
%%-----------------------------------------------------------------------------
-spec get_all_values(Key, List) -> [term()] when
    Key :: term(),
    List :: [term()].

get_all_values(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            [true | get_all_values(Key, Ps)];
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            case P of
                {_, Value} ->
                    [Value | get_all_values(Key, Ps)];
                _ ->
                    get_all_values(Key, Ps)
            end;
        true ->
            get_all_values(Key, Ps)
    end;
get_all_values(_Key, []) ->
    [].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the entry
%% @param   List the property list from which to get the entry
%% @returns Either the found entry (always as a tuple) or `none'
%% @doc     Returns the first entry associated with `Key' in `List', if one exists,
%%          otherwise returns `none'. For an atom `A' in the list, the tuple `{A, true}' is
%%          the entry associated with `A'.
%%          See also `get_bool/2', `get_value/2', `lookup_all/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup(Key, List) -> 'none' | tuple() when
    Key :: term(),
    List :: [term()].

lookup(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            {Key, true};
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            %% Note that <code>Key</code> does not have to be an atom in this case.
            P;
        true ->
            lookup(Key, Ps)
    end;
lookup(_Key, []) ->
    none.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the entries
%% @param   List the property list from which to get the entries
%% @returns all entries having `Key' as key
%% @doc     Returns the list of all entries associated with `Key' in `List'. If no such
%%          entry exists, the result is the empty list.
%%          See also `lookup/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_all(Key, List) -> [tuple()] when
    Key :: term(),
    List :: [term()].

lookup_all(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            [{Key, true} | lookup_all(Key, Ps)];
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            [P | lookup_all(Key, Ps)];
        true ->
            lookup_all(Key, Ps)
    end;
lookup_all(_Key, []) ->
    [].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   List the list will be converted to a map, such as `[key, {one, 1}]'
%% @returns the list converted as a map, such as `#{key => true, one => 1}'
%% @doc     Converts the property list `List' to a map
%%
%%          Shorthand atom values in `List' will be expanded to an association of the form
%%          `Atom => true'. Tuples of the form `{Key, Value}' in `List' will be converted to
%%          an association of the form `Key => Value'. Anything else will be silently
%%          ignored.
%%
%%          If the same key appears in `List' multiple times, the value of the one appearing
%%          nearest to the head of `List' will be in the result map, that is the value that
%%          would be returned by a call to [`get_value(Key, List)'](`get_value/2').
%% @end
%%-----------------------------------------------------------------------------
-spec to_map(List) -> Map when
    List :: [Shorthand | {Key, Value} | term()],
    Map :: #{Shorthand => 'true', Key => Value},
    Shorthand :: atom(),
    Key :: term(),
    Value :: term().

to_map(List) ->
    lists:foldr(
        fun
            ({K, V}, M) ->
                M#{K => V};
            %% if tuples with arity /= 2 appear before atoms or
            %% tuples with arity == 2, get_value/2,3 returns early
            (T, M) when 1 =< tuple_size(T) ->
                maps:remove(element(1, T), M);
            (K, M) when is_atom(K) ->
                M#{K => true};
            (_, M) ->
                M
        end,
        #{},
        List
    ).

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Map the map that will be converted, such as `#{key => true}'
%% @returns the map converted to list, such as `[{key, true}]'
%% @doc     Converts the map `Map' to a property list.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec from_map(Map) -> List when
    Map :: #{Key => Value},
    List :: [{Key, Value}],
    Key :: term(),
    Value :: term().

from_map(Map) ->
    maps:to_list(Map).

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   ListIn the list that will be unfolded, such as `[key]'
%% @returns the unfolded list, such as `{key, true}'
%% @doc     Unfolds all occurrences of atoms in `ListIn' to tuples `{Atom, true}'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec unfold(ListIn) -> ListOut when
    ListIn :: [term()],
    ListOut :: [term()].

unfold([P | Ps]) ->
    if
        is_atom(P) ->
            [{P, true} | unfold(Ps)];
        true ->
            [P | unfold(Ps)]
    end;
unfold([]) ->
    [].

%%-----------------------------------------------------------------------------
%% @param   ListIn the list will be compacted, such as `[{key, true}]'
%% @returns the compacted list, such as `[key]'
%% @doc     Minimizes the representation of all entries in the list. This is equivalent to
%%          `[property(P) || P <- ListIn]'.
%%          See also `property/1', `unfold/1'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec compact(ListIn) -> ListOut when
    ListIn :: [property()],
    ListOut :: [property()].

compact(ListIn) ->
    [property(P) || P <- ListIn].

%%-----------------------------------------------------------------------------
%% @param   Aliases a list of `{Key, Key1}' renames
%% @param   ListIn the list to transform
%% @returns the list with keys substituted
%% @doc     Substitutes keys of properties. For each entry whose key is `Key',
%%          the key is replaced with `Key1'. The resulting property is
%%          minimized (see `property/1').
%%          See also `normalize/2', `substitute_negations/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec substitute_aliases(Aliases, ListIn) -> ListOut when
    Aliases :: [{Key :: term(), Key1 :: term()}],
    ListIn :: [term()],
    ListOut :: [term()].

substitute_aliases(As, Props) ->
    [substitute_aliases_1(As, P) || P <- Props].

substitute_aliases_1([{Key, Key1} | As], P) ->
    if
        is_atom(P), P =:= Key ->
            property(Key1, true);
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            property(setelement(1, P, Key1));
        true ->
            substitute_aliases_1(As, P)
    end;
substitute_aliases_1([], P) ->
    P.

%%-----------------------------------------------------------------------------
%% @param   Negations a list of `{Key, Key1}' negations
%% @param   ListIn the list to transform
%% @returns the list with boolean-valued properties negated and renamed
%% @doc     Substitutes keys of boolean-valued properties and simultaneously
%%          negates their values. Non-boolean tuple values are interpreted as
%%          `false', as done in `get_bool/2'.
%%          See also `normalize/2', `substitute_aliases/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec substitute_negations(Negations, ListIn) -> ListOut when
    Negations :: [{Key :: term(), Key1 :: term()}],
    ListIn :: [term()],
    ListOut :: [term()].

substitute_negations(As, Props) ->
    [substitute_negations_1(As, P) || P <- Props].

substitute_negations_1([{Key, Key1} | As], P) ->
    if
        is_atom(P), P =:= Key ->
            property(Key1, false);
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            case P of
                {_, true} ->
                    property(Key1, false);
                {_, false} ->
                    property(Key1, true);
                _ ->
                    %% The property is supposed to be a boolean, so any
                    %% other tuple is interpreted as `false', as done in
                    %% get_bool/2
                    property(Key1, true)
            end;
        true ->
            substitute_negations_1(As, P)
    end;
substitute_negations_1([], P) ->
    P.

%%-----------------------------------------------------------------------------
%% @param   Expansions a list of `{Property, Expansion}' pairs
%% @param   ListIn the list to transform
%% @returns the expanded list
%% @doc     Expands particular properties to corresponding sets of properties.
%%          The first occurrence of a property whose minimal representation
%%          matches `Property' is replaced by `Expansion' and any following
%%          entries with the same key are deleted.
%%          See also `normalize/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec expand(Expansions, ListIn) -> ListOut when
    Expansions :: [{Property :: property(), Expansion :: [term()]}],
    ListIn :: [term()],
    ListOut :: [term()].

expand(Es, Ps) when is_list(Ps) ->
    Es1 = [{property(P), V} || {P, V} <- Es],
    expand_flatten(expand_0(expand_key_uniq(Es1), Ps)).

expand_0([{P, L} | Es], Ps) ->
    expand_0(Es, expand_1(P, L, Ps));
expand_0([], Ps) ->
    Ps.

expand_1(P, L, Ps) ->
    %% P has a minimal representation here.
    if
        is_atom(P) ->
            expand_2(P, P, L, Ps);
        tuple_size(P) >= 1 ->
            expand_2(element(1, P), P, L, Ps);
        % refuse to expand non-property
        true ->
            Ps
    end.

expand_2(Key, P1, L, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            expand_3(Key, P1, P, L, Ps);
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            expand_3(Key, P1, property(P), L, Ps);
        true ->
            %% Non-property entries and already inserted expansions
            %% (lists) are ignored.
            [P | expand_2(Key, P1, L, Ps)]
    end;
expand_2(_, _, _, []) ->
    [].

expand_3(Key, P1, P, L, Ps) ->
    %% Both P and P1 have minimal representations here. The inserted
    %% list is flattened afterwards. If the expansion is done, the found
    %% entry is dropped along with any later entries with the same key.
    if
        P1 =:= P ->
            [L | delete(Key, Ps)];
        true ->
            %% The existing entry does not match - keep it.
            [P | Ps]
    end.

expand_key_uniq([{K, V} | Ps]) ->
    [{K, V} | expand_key_uniq_1(K, Ps)];
expand_key_uniq([]) ->
    [].

expand_key_uniq_1(K, [{K1, V} | Ps]) ->
    if
        K =:= K1 ->
            expand_key_uniq_1(K, Ps);
        true ->
            [{K1, V} | expand_key_uniq_1(K1, Ps)]
    end;
expand_key_uniq_1(_, []) ->
    [].

expand_flatten([E | Es]) when is_list(E) ->
    E ++ expand_flatten(Es);
expand_flatten([E | Es]) ->
    [E | expand_flatten(Es)];
expand_flatten([]) ->
    [].

%%-----------------------------------------------------------------------------
%% @param   ListIn the list to normalize
%% @param   Stages a list of `{aliases, As}', `{negations, Ns}' and
%%          `{expand, Es}' operations
%% @returns the normalized list
%% @doc     Passes `ListIn' through a sequence of substitution/expansion
%%          stages, then compacts the result (see `compact/1').
%%          See also `substitute_aliases/2', `substitute_negations/2',
%%          `expand/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec normalize(ListIn, Stages) -> ListOut when
    ListIn :: [term()],
    Stages :: [Operation],
    Operation ::
        {aliases, [{Key :: term(), Key1 :: term()}]}
        | {negations, [{Key :: term(), Key1 :: term()}]}
        | {expand, [{Property :: property(), Expansion :: [term()]}]},
    ListOut :: [term()].

normalize(L, Stages) ->
    compact(apply_stages(L, Stages)).

apply_stages(L, [{aliases, As} | Xs]) ->
    apply_stages(substitute_aliases(As, L), Xs);
apply_stages(L, [{expand, Es} | Xs]) ->
    apply_stages(expand(Es, L), Xs);
apply_stages(L, [{negations, Ns} | Xs]) ->
    apply_stages(substitute_negations(Ns, L), Xs);
apply_stages(L, []) ->
    L.
