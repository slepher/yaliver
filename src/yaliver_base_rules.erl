%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(yaliver_base_rules).

%% API
-export([required/3, default/3]).
-export([not_empty/2, not_empty_list/2]).
-export([eq/2, one_of/2]).
-export([any_object/2]).
-export(['or'/3, 'and'/3]).
-export([validator/3]).

%%%===================================================================
%%% API
%%%===================================================================
required(_Args, Value, #{key := Key, parent := Parent}) ->
    case maps:is_key(Key, Parent) of
        false ->
            {error, required};
        true ->
            {ok, Value}
    end;
required(_Args, undefined, _Options) ->
    {ok, required_not_in_map}.

default([Default], Value, #{key := Key, parent := Parent}) ->
    case maps:is_key(Key, Parent) of
        false ->
            {ok, Default};
        true ->
            {ok, Value}
    end;
default(_Args, undefined, _Options) ->
    {ok, default_not_in_map}.

not_empty(_Args, undefined) ->
    {error, cannot_be_empty};
not_empty(_Args, []) ->
    {error, cannot_be_empty};
not_empty(_Args, Value) ->
    {ok, Value}.

not_empty_list(_Args, [_A|_T] = List) ->
    {ok, List};
not_empty_list(_Args, _NotEmptyList) ->
    {error, not_empty_list}.

eq([Value], Value) ->
    {ok, Value};
eq([_Value1], _Value) ->
    {error, not_allowed_value};
eq(_Args, _Value) ->
    {error, format_error}.

one_of([Values], Value) when is_list(Values) ->
    case lists:member(Value, Values) of
        true ->
            {ok, Value};
        false ->
            {error, not_allowed_value}
    end;
one_of(Values, Value) when is_list(Values) ->
    one_of([Values], Value);
one_of(_Args, _Value) ->
    {error, format_error}.

any_object([], Value) ->
    {ok, Value}.

'or'([_Validator|_T] = Validators, Value, Options) ->
    or_1(Validators, Value, Options, []);
'or'([], _Value, _Options) ->
    {error, format_error}.

'and'([_Validator|_T] = Validators, Value, Options) ->
    and_1(Validators, Value, Options);
'and'([], _Value, _Options) ->
    {error, format_error}.    

validator([Function], Value, _Options) when is_function(Function, 1) ->
    Function(Value);
validator([Function, Args], Value, _Options) when is_function(Function, 2), is_list(Args) ->
    Function(Args, Value);
validator([Function, Args], Value, Options) when is_function(Function, 3), is_list(Args) ->
    Function(Args, Value, Options);
validator(_Args, _Value, _Options) ->
    {error, format_error}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
or_1([Validator|T], Value, Options, Errors) ->
    case yaliver:validate_1(Validator, Value, Options) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            or_1(T, Value, Options, [Reason|Errors])
    end;
or_1([], _Value, _Options, Errors) ->
    {error, {all_not_match, Errors}}.

and_1([Validator|T], Value, Options) ->
    case yaliver:validate_1(Validator, Value, Options) of
        {ok, Value1} ->
            and_1(T, Value1, Options);
        {error, Reason} ->
            {error, Reason}
    end;
and_1([], Value, _Options) ->
    {ok, Value}.
