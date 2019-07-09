%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(yaliver_string_rules).

-include_lib("erlando/include/do.hrl").

%% API
-export([string/1]).
-export([max_length/2, min_length/2, length_between/2]).
-export([like/2]).
-export([trim/1, to_lc/1, to_uc/1]).
%%%===================================================================
%%% API
%%%===================================================================
string(Value) when is_binary(Value) ->
    {ok, Value};
string(Value) when is_number(Value) ->
    {ok, integer_to_binary(Value)};
string(Value) when is_atom(Value) ->
    {ok, atom_to_binary(Value, utf8)};
string(_Value) ->
    {error, invalid_format}.

max_length([MaxLength] = Args, Value) when is_binary(Value), is_integer(MaxLength) ->
    max_length_1(Args, Value);
max_length(_Args, _Value) ->
    {error, format_error}.

min_length([MinLength] = Args, Value) when is_binary(Value), is_integer(MinLength) ->
    min_length_1(Args, Value);
min_length(_Args, _Value) ->
    {error, format_error}.

length_between([MinLength, MaxLength], Value) when is_binary(Value), is_integer(MinLength), is_integer(MaxLength) ->
    length_between_1(MinLength, MaxLength, Value);
length_between(_Args, Value) when is_binary(Value) ->
    {error, format_error}.

like([Regex, i], Value) ->
    like_1(Regex, [unicode, caseless], Value);
like([Regex], Value) ->
    like_1(Regex, [unicode, caseless], Value);
like(_Args, _Value) ->
    {error, format_error}.

trim(Value) when is_binary(Value) ->
    string:trim(Value);
trim(Value) ->
    do([error_m ||
           String <- string(Value),
           trim(String)
       ]).

to_lc(Value) when is_binary(Value) ->
    string:lowercase(Value);
to_lc(Value) ->
    do([error_m ||
           String <- string(Value),
           to_lc(String)
       ]).

to_uc(Value) when is_binary(Value) ->
    string:uppercase(Value);
to_uc(Value) ->
    do([error_m ||
           String <- string(Value),
           to_uc(String)
       ]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
max_length_1(MaxLength, Value) when is_binary(Value) ->
    StrLength = length(unicode:characters_to_list(Value)),
    case StrLength > MaxLength of
        false   -> {ok, Value};
        true    -> {error, too_long}
    end;
max_length_1(MaxLength, Value) ->
    do([error_m ||
           String <- string(Value),
           max_length_1(MaxLength, String)
       ]).

min_length_1(MinLength, Value) when is_binary(Value) ->
    StrLength = length(unicode:characters_to_list(Value)),
    case StrLength < MinLength of
        false   -> {ok, Value};
        true    -> {error, too_long}
    end;
min_length_1(MinLength, Value) ->
    do([error_m ||
           String <- string(Value),
           min_length_1(MinLength, String)
       ]).

length_between_1(MinLength, MaxLength, Value) when is_binary(Value) ->
    StrLength = length(unicode:characters_to_list(Value)),
    if 
        StrLength < MinLength -> {error, too_short};
        StrLength > MaxLength -> {error, too_long};
        true                  -> {ok, Value}
    end;
length_between_1(MinLength, MaxLength, Value) ->
    do([error_m ||
           String <- string(Value),
           length_between_1(MinLength, MaxLength, String)
       ]).

like_1(Pattern, PatternOpts, Value) when is_binary(Value) ->
    case re:compile(Pattern, PatternOpts) of
        {ok, MP} ->
            case re:run(Value, MP) of
                nomatch -> {error, wrong_format};
                _       -> {ok, Value}
            end;
        {error, _Reason} ->
            {error, invalid_pattern}
    end;
like_1(Pattern, PatternOpts, Value) ->
    do([error_m ||
           String <- string(Value),
           like_1(Pattern, PatternOpts, String)
       ]).

