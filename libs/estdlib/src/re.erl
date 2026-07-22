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
%% @doc An implementation of a commonly used subset of the Erlang/OTP `re'
%% interface on top of PCRE2 (when the VM is built with it).
%%
%% The `mp()' pattern is `{re_pattern, CaptureCount, Unicode, 0, Data}' where
%% `Data' packs the name table and the PCRE2 serialized pattern. The format
%% is AtomVM-specific: patterns compiled by Erlang/OTP cannot be executed
%% here nor vice versa, but callers that recompile from source (as Elixir
%% does on Erlang/OTP 28+) behave identically.
%% @end
%%-----------------------------------------------------------------------------
-module(re).

-export([
    version/0,
    compile/1, compile/2,
    run/2, run/3,
    internal_run/4,
    replace/3, replace/4,
    split/2, split/3,
    inspect/2,
    import/1
]).

%% Low-level PCRE2 primitives (NIFs)
-export([pcre2_compile/2, pcre2_match/4]).

%% PCRE2 compile option bits
-define(PCRE2_ANCHORED, 16#80000000).
-define(PCRE2_CASELESS, 16#00000008).
-define(PCRE2_DOLLAR_ENDONLY, 16#00000010).
-define(PCRE2_DOTALL, 16#00000020).
-define(PCRE2_DUPNAMES, 16#00000040).
-define(PCRE2_EXTENDED, 16#00000080).
-define(PCRE2_FIRSTLINE, 16#00000100).
-define(PCRE2_MULTILINE, 16#00000400).
-define(PCRE2_NEVER_UTF, 16#00001000).
-define(PCRE2_NO_AUTO_CAPTURE, 16#00002000).
-define(PCRE2_NO_START_OPTIMIZE, 16#00010000).
-define(PCRE2_UCP, 16#00020000).
-define(PCRE2_UNGREEDY, 16#00040000).
-define(PCRE2_UTF, 16#00080000).

%% PCRE2 match option bits
-define(PCRE2_NOTBOL, 16#00000001).
-define(PCRE2_NOTEOL, 16#00000002).
-define(PCRE2_NOTEMPTY, 16#00000004).
-define(PCRE2_NOTEMPTY_ATSTART, 16#00000008).

%%-----------------------------------------------------------------------------
%% @returns the version of the underlying regular expression engine
%% @doc     Version of the underlying PCRE2 library, in an AtomVM-specific
%% rendering (never equal to an Erlang/OTP PCRE version string, so version
%% aware callers recompile their patterns from source).
%% @end
%%-----------------------------------------------------------------------------
-spec version() -> binary().
version() ->
    <<"AtomVM PCRE2">>.

%% @equiv compile(Regexp, [])
-spec compile(Regexp :: iodata()) -> {ok, tuple()} | {error, {binary(), non_neg_integer()}}.
compile(Regexp) ->
    compile(Regexp, []).

%%-----------------------------------------------------------------------------
%% @param   Regexp the regular expression, as iodata or a charlist
%% @param   Options compile options
%% @returns `{ok, MP}' or `{error, {Message, Position}}'
%% @doc     Compile a regular expression.
%% @end
%%-----------------------------------------------------------------------------
-spec compile(Regexp :: iodata(), Options :: [term()]) ->
    {ok, tuple()} | {error, {binary(), non_neg_integer()}}.
compile(Regexp, Options) ->
    Source = characters_to_binary(Regexp, in_unicode(Options)),
    Flags = compile_flags(Options, 0),
    case ?MODULE:pcre2_compile(Source, Flags) of
        {ok, CaptureCount, NameCount, NameEntrySize, NameTable, Serialized} ->
            Unicode =
                case Flags band ?PCRE2_UTF of
                    0 -> 0;
                    _ -> 1
                end,
            %% Pack everything into one binary so the mp() shape matches
            %% Erlang/OTP's {re_pattern, _, _, _, binary()} (version-aware
            %% callers sniff the shape).
            Data =
                <<NameCount:16, NameEntrySize:16, (byte_size(NameTable)):32, NameTable/binary,
                    Serialized/binary>>,
            MP = {re_pattern, CaptureCount, Unicode, 0, Data},
            case lists:member(export, Options) of
                false ->
                    {ok, MP};
                true ->
                    %% Exported form, as on Erlang/OTP 28.1+: carries the
                    %% source and options so an incompatible importer can
                    %% recompile (see import/1).
                    Exported =
                        {re_exported_pattern, version(), Source, Options,
                            <<CaptureCount:32, Unicode:8, Data/binary>>},
                    {ok, Exported}
            end;
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Exported a pattern compiled with the `export' option
%% @returns the imported pattern, as `mp()'
%% @doc     Import a regular expression compiled with the `export' option.
%% When the exporter is not this same engine version, the expression is
%% recompiled from the carried source and options.
%% @end
%%-----------------------------------------------------------------------------
-spec import(Exported :: tuple()) -> tuple().
import({re_exported_pattern, Version, Source, Options, ExportData}) ->
    case Version =:= version() of
        true ->
            <<CaptureCount:32, Unicode:8, Data/binary>> = ExportData,
            {re_pattern, CaptureCount, Unicode, 0, Data};
        false ->
            case compile(Source, Options -- [export]) of
                {ok, MP} -> MP;
                {error, _} -> erlang:error(badarg)
            end
    end.

%% @equiv run(Subject, RE, [])
-spec run(Subject :: iodata(), RE :: iodata() | tuple()) -> {match, list()} | nomatch.
run(Subject, RE) ->
    run(Subject, RE, []).

%%-----------------------------------------------------------------------------
%% @param   Subject the subject, as iodata or a charlist
%% @param   RE a compiled pattern or a regular expression source
%% @param   Options run options
%% @returns `{match, Captured}', `nomatch' or `{error, ErrType}'
%% @doc     Match a subject against a regular expression.
%%
%% Supported options: `anchored', `global', `notbol', `noteol', `notempty',
%% `notempty_atstart', `{offset, Offset}', `{capture, ValueSpec}',
%% `{capture, ValueSpec, Type}' and all compile options (the expression is
%% compiled on the fly when a source is given).
%% @end
%%-----------------------------------------------------------------------------
-spec run(Subject :: iodata(), RE :: iodata() | tuple(), Options :: [term()]) ->
    {match, list()} | nomatch | {error, term()}.
run(Subject, RE, Options) ->
    {MP, RunOptions} = ensure_compiled(RE, Options),
    {re_pattern, CaptureCount, Unicode, 0, Data} = MP,
    <<NameCount:16, NameEntrySize:16, TableSize:32, NameTable:TableSize/binary, Serialized/binary>> =
        Data,
    IsUnicode = Unicode =:= 1,
    {SubjectBin, _WasCharlist} = subject_to_binary(Subject, IsUnicode),
    Offset = proplists:get_value(offset, RunOptions, 0),
    MatchFlags = match_flags(RunOptions, 0),
    Global = lists:member(global, RunOptions),
    RawMatches =
        case Global of
            false ->
                case ?MODULE:pcre2_match(SubjectBin, Serialized, Offset, MatchFlags) of
                    {match, Pairs} -> [pairs_to_tuples(Pairs)];
                    nomatch -> [];
                    {error, _} = MatchError -> throw_or_return(MatchError)
                end;
            true ->
                global_loop(SubjectBin, Serialized, Offset, MatchFlags, IsUnicode, [])
        end,
    case RawMatches of
        [] ->
            nomatch;
        {error, _} = RunError ->
            RunError;
        _ ->
            {CaptureSpec, CaptureType} = capture_options(RunOptions),
            case CaptureSpec of
                none ->
                    match;
                _ ->
                    Names = decode_name_table(NameCount, NameEntrySize, NameTable),
                    Captured = [
                        format_capture(
                            M, CaptureSpec, CaptureType, SubjectBin, CaptureCount, Names
                        )
                     || M <- RawMatches
                    ],
                    case Global of
                        true -> {match, Captured};
                        false -> {match, hd(Captured)}
                    end
            end
    end.

%%-----------------------------------------------------------------------------
%% @doc     Like `run/3'; the extra argument is accepted for Erlang/OTP
%% compatibility (it modifies error reporting on OTP, which is not
%% implemented here).
%% @end
%%-----------------------------------------------------------------------------
-spec internal_run(Subject :: iodata(), RE :: iodata() | tuple(), Options :: [term()], boolean()) ->
    {match, list()} | nomatch | {error, term()}.
internal_run(Subject, RE, Options, _FirstCall) ->
    run(Subject, RE, Options).

%% @equiv replace(Subject, RE, Replacement, [])
-spec replace(Subject :: iodata(), RE :: iodata() | tuple(), Replacement :: iodata() | function()) ->
    iodata().
replace(Subject, RE, Replacement) ->
    replace(Subject, RE, Replacement, []).

%%-----------------------------------------------------------------------------
%% @param   Subject the subject
%% @param   RE a compiled pattern or a regular expression source
%% @param   Replacement a replacement template (`\N', `\g{N}', `&' and `\\'
%%          are honored) or a fun returning iodata
%% @param   Options run options plus `{return, list | binary | iodata}'
%% @returns the subject with matches replaced
%% @doc     Replace matches of a regular expression.
%% @end
%%-----------------------------------------------------------------------------
-spec replace(
    Subject :: iodata(),
    RE :: iodata() | tuple(),
    Replacement :: iodata() | function(),
    Options :: [term()]
) -> iodata().
replace(Subject, RE, Replacement, Options) ->
    {ReturnType, RunOptions0} = take_return_option(Options, iodata),
    Global = lists:member(global, RunOptions0),
    RunOptions1 = [{capture, all, index} | RunOptions0 -- [global]],
    {MP, _} = ensure_compiled(RE, RunOptions1),
    {re_pattern, _, Unicode, 0, _} = MP,
    IsUnicode = Unicode =:= 1,
    {SubjectBin, _} = subject_to_binary(Subject, IsUnicode),
    Result = replace_loop(SubjectBin, MP, Replacement, RunOptions1, Global, 0, []),
    return_as(Result, ReturnType, IsUnicode).

%% @equiv split(Subject, RE, [])
-spec split(Subject :: iodata(), RE :: iodata() | tuple()) -> [iodata()].
split(Subject, RE) ->
    split(Subject, RE, []).

%%-----------------------------------------------------------------------------
%% @param   Subject the subject
%% @param   RE a compiled pattern or a regular expression source
%% @param   Options run options plus `{return, list | binary | iodata}',
%%          `{parts, N | infinity}' and `trim'
%% @returns the subject parts split around matches
%% @doc     Split a subject by the matches of a regular expression. Captured
%% groups are returned between parts, as on Erlang/OTP.
%% @end
%%-----------------------------------------------------------------------------
-spec split(Subject :: iodata(), RE :: iodata() | tuple(), Options :: [term()]) -> [iodata()].
split(Subject, RE, Options) ->
    {ReturnType, Options1} = take_return_option(Options, iodata),
    {Parts, Options2} = take_parts_option(Options1),
    Trim = lists:member(trim, Options2),
    RunOptions = [global, {capture, all, index} | Options2 -- [trim]],
    {MP, _} = ensure_compiled(RE, RunOptions),
    {re_pattern, _, Unicode, 0, _} = MP,
    IsUnicode = Unicode =:= 1,
    {SubjectBin, _} = subject_to_binary(Subject, IsUnicode),
    Fields =
        case run(SubjectBin, MP, RunOptions) of
            nomatch ->
                [SubjectBin];
            {match, Matches} ->
                split_fields(SubjectBin, Matches, Parts, 1, 0, [])
        end,
    Trimmed =
        case Trim of
            true -> trim_empty_tail(Fields);
            false when Parts =:= 0 -> trim_empty_tail(Fields);
            false -> Fields
        end,
    [return_as(F, ReturnType, IsUnicode) || F <- Trimmed].

%%-----------------------------------------------------------------------------
%% @param   MP a compiled pattern
%% @param   Item `namelist' is supported
%% @returns inspection data for a compiled pattern
%% @doc     Inspect a compiled regular expression.
%% @end
%%-----------------------------------------------------------------------------
-spec inspect(MP :: tuple(), Item :: namelist) -> {namelist, [binary()]}.
inspect({re_pattern, _, _, 0, Data}, namelist) ->
    <<NameCount:16, NameEntrySize:16, TableSize:32, NameTable:TableSize/binary, _/binary>> = Data,
    Names = decode_name_table(NameCount, NameEntrySize, NameTable),
    {namelist, lists:sort([N || {N, _} <- Names])}.

%% Low-level PCRE2 primitives (NIFs)

%% @private
pcre2_compile(_Source, _Flags) ->
    erlang:nif_error(undefined).

%% @private
pcre2_match(_Subject, _Serialized, _Offset, _Flags) ->
    erlang:nif_error(undefined).

%%
%% Internal functions
%%

%% @private
ensure_compiled({re_pattern, _, _, 0, _} = MP, Options) ->
    {MP, Options};
ensure_compiled({re_exported_pattern, _, _, _, _} = Exported, Options) ->
    {?MODULE:import(Exported), Options};
ensure_compiled(Source, Options) ->
    CompileOptions = [O || O <- Options, is_compile_option(O)],
    case compile(Source, CompileOptions) of
        {ok, MP} -> {MP, Options -- CompileOptions};
        {error, Reason} -> error({badmatch, {error, Reason}})
    end.

%% @private
is_compile_option(unicode) -> true;
is_compile_option(caseless) -> true;
is_compile_option(dotall) -> true;
is_compile_option(multiline) -> true;
is_compile_option(extended) -> true;
is_compile_option(firstline) -> true;
is_compile_option(ungreedy) -> true;
is_compile_option(no_auto_capture) -> true;
is_compile_option(dupnames) -> true;
is_compile_option(dollar_endonly) -> true;
is_compile_option(ucp) -> true;
is_compile_option(never_utf) -> true;
is_compile_option(no_start_optimize) -> true;
is_compile_option(_) -> false.

%% @private
compile_flags([], Acc) ->
    Acc;
compile_flags([unicode | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_UTF bor ?PCRE2_UCP);
compile_flags([caseless | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_CASELESS);
compile_flags([dotall | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_DOTALL);
compile_flags([multiline | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_MULTILINE);
compile_flags([extended | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_EXTENDED);
compile_flags([firstline | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_FIRSTLINE);
compile_flags([ungreedy | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_UNGREEDY);
compile_flags([no_auto_capture | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_NO_AUTO_CAPTURE);
compile_flags([dupnames | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_DUPNAMES);
compile_flags([dollar_endonly | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_DOLLAR_ENDONLY);
compile_flags([ucp | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_UCP);
compile_flags([never_utf | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_NEVER_UTF);
compile_flags([no_start_optimize | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_NO_START_OPTIMIZE);
compile_flags([anchored | T], Acc) ->
    compile_flags(T, Acc bor ?PCRE2_ANCHORED);
compile_flags([_ | T], Acc) ->
    compile_flags(T, Acc).

%% @private
match_flags([], Acc) -> Acc;
match_flags([anchored | T], Acc) -> match_flags(T, Acc bor ?PCRE2_ANCHORED);
match_flags([notbol | T], Acc) -> match_flags(T, Acc bor ?PCRE2_NOTBOL);
match_flags([noteol | T], Acc) -> match_flags(T, Acc bor ?PCRE2_NOTEOL);
match_flags([notempty | T], Acc) -> match_flags(T, Acc bor ?PCRE2_NOTEMPTY);
match_flags([notempty_atstart | T], Acc) -> match_flags(T, Acc bor ?PCRE2_NOTEMPTY_ATSTART);
match_flags([_ | T], Acc) -> match_flags(T, Acc).

%% @private
in_unicode(Options) ->
    lists:member(unicode, Options).

%% @private
characters_to_binary(Data, _Unicode) when is_binary(Data) ->
    Data;
characters_to_binary(Data, true) ->
    unicode:characters_to_binary(Data);
characters_to_binary(Data, false) ->
    % latin1 charlists: bytes as-is
    erlang:iolist_to_binary(chars_to_bytes(Data)).

%% @private
%% Charlists in non-unicode mode may still hold codepoints < 256 only; a
%% plain iolist_to_binary handles those. Codepoints >= 256 require unicode.
chars_to_bytes(Data) ->
    Data.

%% @private
subject_to_binary(Subject, _IsUnicode) when is_binary(Subject) ->
    {Subject, false};
subject_to_binary(Subject, IsUnicode) ->
    % A charlist (or deep iodata) subject: captures default to charlists,
    % as on Erlang/OTP.
    Bin =
        case IsUnicode of
            true -> unicode:characters_to_binary(Subject);
            false -> erlang:iolist_to_binary(Subject)
        end,
    {Bin, is_list(Subject)}.

%% @private
pairs_to_tuples([]) -> [];
pairs_to_tuples([S, L | T]) -> [{S, L} | pairs_to_tuples(T)].

%% @private
global_loop(SubjectBin, Serialized, Offset, MatchFlags, IsUnicode, Acc) when
    Offset =< byte_size(SubjectBin)
->
    case ?MODULE:pcre2_match(SubjectBin, Serialized, Offset, MatchFlags) of
        {match, [S, L | _] = Pairs} ->
            Match = pairs_to_tuples(Pairs),
            NextOffset =
                case L of
                    0 ->
                        % empty match: retry at the same position requiring a
                        % non-empty match, or advance one character
                        case
                            ?MODULE:pcre2_match(
                                SubjectBin,
                                Serialized,
                                S,
                                MatchFlags bor ?PCRE2_NOTEMPTY_ATSTART
                            )
                        of
                            {match, [S2, L2 | _] = Pairs2} when L2 > 0 ->
                                {retry, S2 + L2, pairs_to_tuples(Pairs2)};
                            _ ->
                                S + char_size(SubjectBin, S, IsUnicode)
                        end;
                    _ ->
                        S + L
                end,
            case NextOffset of
                {retry, NO, Match2} ->
                    global_loop(SubjectBin, Serialized, NO, MatchFlags, IsUnicode, [
                        Match2, Match | Acc
                    ]);
                NO when is_integer(NO) ->
                    global_loop(SubjectBin, Serialized, NO, MatchFlags, IsUnicode, [Match | Acc])
            end;
        nomatch ->
            lists:reverse(Acc);
        {error, _} = Error ->
            Error
    end;
global_loop(_SubjectBin, _Serialized, _Offset, _MatchFlags, _IsUnicode, Acc) ->
    lists:reverse(Acc).

%% @private
char_size(SubjectBin, Offset, true) when Offset < byte_size(SubjectBin) ->
    % advance one UTF-8 character
    case binary:at(SubjectBin, Offset) of
        B when B < 16#80 -> 1;
        B when B < 16#E0 -> 2;
        B when B < 16#F0 -> 3;
        _ -> 4
    end;
char_size(_SubjectBin, _Offset, _) ->
    1.

%% @private
capture_options(Options) ->
    case lists:keyfind(capture, 1, Options) of
        {capture, Spec} -> {Spec, index};
        {capture, Spec, Type} -> {Spec, Type};
        false -> {all, index}
    end.

%% @private
decode_name_table(0, _EntrySize, _Table) ->
    [];
decode_name_table(Count, EntrySize, Table) ->
    [
        begin
            Entry = binary:part(Table, I * EntrySize, EntrySize),
            <<Index:16, NameZ/binary>> = Entry,
            {trim_nul(NameZ), Index}
        end
     || I <- lists:seq(0, Count - 1)
    ].

%% @private
trim_nul(Bin) ->
    case binary:split(Bin, <<0>>) of
        [Name | _] -> Name;
        [] -> Bin
    end.

%% @private
format_capture(Match, all, Type, SubjectBin, _CaptureCount, _Names) ->
    [capture_value(T, Type, SubjectBin) || T <- Match];
format_capture(Match, first, Type, SubjectBin, _CaptureCount, _Names) ->
    [capture_value(hd(Match), Type, SubjectBin)];
format_capture(Match, all_but_first, Type, SubjectBin, _CaptureCount, _Names) ->
    [capture_value(T, Type, SubjectBin) || T <- tl(Match)];
format_capture(Match, all_names, Type, SubjectBin, _CaptureCount, Names) ->
    Sorted = lists:sort([N || {N, _} <- Names]),
    [
        capture_value(nth_or_unset(index_of_name(N, Names), Match), Type, SubjectBin)
     || N <- Sorted
    ];
format_capture(Match, List, Type, SubjectBin, _CaptureCount, Names) when is_list(List) ->
    [
        capture_value(nth_or_unset(capture_index(Item, Names), Match), Type, SubjectBin)
     || Item <- List
    ].

%% @private
capture_index(N, _Names) when is_integer(N) ->
    N;
capture_index(Name, Names) when is_atom(Name) ->
    index_of_name(atom_to_binary(Name, utf8), Names);
capture_index(Name, Names) when is_binary(Name) ->
    index_of_name(Name, Names);
capture_index(Name, Names) when is_list(Name) ->
    index_of_name(list_to_binary(Name), Names).

%% @private
index_of_name(Name, Names) ->
    case lists:keyfind(Name, 1, Names) of
        {_, Index} -> Index;
        false -> -1
    end.

%% @private
nth_or_unset(Index, Match) when Index >= 0, Index < length(Match) ->
    lists:nth(Index + 1, Match);
nth_or_unset(_, _) ->
    {-1, 0}.

%% @private
capture_value({-1, 0}, index, _SubjectBin) -> {-1, 0};
capture_value({-1, 0}, binary, _SubjectBin) -> <<>>;
capture_value({-1, 0}, list, _SubjectBin) -> [];
capture_value({S, L}, index, _SubjectBin) -> {S, L};
capture_value({S, L}, binary, SubjectBin) -> binary:part(SubjectBin, S, L);
capture_value({S, L}, list, SubjectBin) -> binary_to_list(binary:part(SubjectBin, S, L)).

%% @private
take_return_option(Options, Default) ->
    case lists:keyfind(return, 1, Options) of
        {return, Type} -> {Type, lists:keydelete(return, 1, Options)};
        false -> {Default, Options}
    end.

%% @private
take_parts_option(Options) ->
    case lists:keyfind(parts, 1, Options) of
        {parts, infinity} -> {0, lists:keydelete(parts, 1, Options)};
        {parts, N} -> {N, lists:keydelete(parts, 1, Options)};
        false -> {0, Options}
    end.

%% @private
replace_loop(SubjectBin, MP, Replacement, RunOptions, Global, Offset, Acc) when
    Offset =< byte_size(SubjectBin)
->
    case run(SubjectBin, MP, [{offset, Offset} | RunOptions]) of
        nomatch ->
            lists:reverse([binary:part(SubjectBin, Offset, byte_size(SubjectBin) - Offset) | Acc]);
        {match, [{S, L} | _] = Match} ->
            Prefix = binary:part(SubjectBin, Offset, S - Offset),
            Replaced = apply_replacement(Replacement, Match, SubjectBin),
            NewAcc = [Replaced, Prefix | Acc],
            case Global of
                false ->
                    lists:reverse([
                        binary:part(SubjectBin, S + L, byte_size(SubjectBin) - S - L) | NewAcc
                    ]);
                true when L =:= 0 ->
                    case S + L < byte_size(SubjectBin) of
                        true ->
                            Char = binary:part(SubjectBin, S, 1),
                            replace_loop(
                                SubjectBin, MP, Replacement, RunOptions, Global, S + 1, [
                                    Char | NewAcc
                                ]
                            );
                        false ->
                            lists:reverse(NewAcc)
                    end;
                true ->
                    replace_loop(SubjectBin, MP, Replacement, RunOptions, Global, S + L, NewAcc)
            end
    end;
replace_loop(_SubjectBin, _MP, _Replacement, _RunOptions, _Global, _Offset, Acc) ->
    lists:reverse(Acc).

%% @private
apply_replacement(Fun, [{S, L} | Groups], SubjectBin) when is_function(Fun) ->
    Whole = binary:part(SubjectBin, S, L),
    GroupBins = [capture_value(G, binary, SubjectBin) || G <- Groups],
    Fun(Whole, GroupBins);
apply_replacement(Template, Match, SubjectBin) ->
    TemplateBin = erlang:iolist_to_binary(Template),
    expand_template(TemplateBin, Match, SubjectBin, <<>>).

%% @private
expand_template(<<>>, _Match, _SubjectBin, Acc) ->
    Acc;
expand_template(<<$\\, $g, ${, Rest/binary>>, Match, SubjectBin, Acc) ->
    {NumBin, <<$}, Rest2/binary>>} = split_digits(Rest, <<>>),
    Group = group_value(binary_to_integer(NumBin), Match, SubjectBin),
    expand_template(Rest2, Match, SubjectBin, <<Acc/binary, Group/binary>>);
expand_template(<<$\\, D, Rest0/binary>>, Match, SubjectBin, Acc) when D >= $0, D =< $9 ->
    {NumBin, Rest} = split_digits(Rest0, <<D>>),
    Group = group_value(binary_to_integer(NumBin), Match, SubjectBin),
    expand_template(Rest, Match, SubjectBin, <<Acc/binary, Group/binary>>);
expand_template(<<$\\, C, Rest/binary>>, Match, SubjectBin, Acc) ->
    expand_template(Rest, Match, SubjectBin, <<Acc/binary, C>>);
expand_template(<<$&, Rest/binary>>, Match, SubjectBin, Acc) ->
    Group = group_value(0, Match, SubjectBin),
    expand_template(Rest, Match, SubjectBin, <<Acc/binary, Group/binary>>);
expand_template(<<C, Rest/binary>>, Match, SubjectBin, Acc) ->
    expand_template(Rest, Match, SubjectBin, <<Acc/binary, C>>).

%% @private
split_digits(<<D, Rest/binary>>, Acc) when D >= $0, D =< $9 ->
    split_digits(Rest, <<Acc/binary, D>>);
split_digits(Rest, Acc) ->
    {Acc, Rest}.

%% @private
group_value(N, Match, SubjectBin) ->
    capture_value(nth_or_unset(N, Match), binary, SubjectBin).

%% @private
%% FieldCount counts the fields (not captured groups) emitted so far plus
%% the pending remainder; {parts, N} caps it at N.
split_fields(SubjectBin, [], _Parts, _FieldCount, Offset, Acc) ->
    lists:reverse([binary:part(SubjectBin, Offset, byte_size(SubjectBin) - Offset) | Acc]);
split_fields(SubjectBin, [[{S, L} | Groups] | T], Parts, FieldCount, Offset, Acc) ->
    case Parts =/= 0 andalso FieldCount >= Parts of
        true ->
            lists:reverse([binary:part(SubjectBin, Offset, byte_size(SubjectBin) - Offset) | Acc]);
        false ->
            Field = binary:part(SubjectBin, Offset, S - Offset),
            GroupFields = [capture_value(G, binary, SubjectBin) || G <- Groups],
            split_fields(
                SubjectBin,
                T,
                Parts,
                FieldCount + 1,
                S + L,
                lists:reverse(GroupFields) ++ [Field | Acc]
            )
    end.

%% @private
trim_empty_tail(Fields) ->
    lists:reverse(drop_empty(lists:reverse(Fields))).

%% @private
drop_empty([<<>> | T]) -> drop_empty(T);
drop_empty([[] | T]) -> drop_empty(T);
drop_empty(L) -> L.

%% @private
return_as(IodataOrBin, iodata, _IsUnicode) ->
    IodataOrBin;
return_as(IodataOrBin, binary, _IsUnicode) ->
    erlang:iolist_to_binary(IodataOrBin);
return_as(IodataOrBin, list, IsUnicode) ->
    Bin = erlang:iolist_to_binary(IodataOrBin),
    case IsUnicode of
        true -> unicode:characters_to_list(Bin);
        false -> binary_to_list(Bin)
    end.

%% @private
throw_or_return({error, _} = Error) ->
    Error.
