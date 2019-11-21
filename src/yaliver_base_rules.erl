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
-export([required/4, default/4]).
-export([not_empty/2, not_empty_list/2]).
-export([eq/2, one_of/2]).
-export([any_object/2]).
-export(['or'/3, 'and'/4]).
-export([validator/3, validator/4]).

%%%===================================================================
%%% API
%%%===================================================================
required(_Args, undefined, #{is_key := false}, _Opts) ->
    {error, required};
required(_Args, Value, #{is_key := true}, _Opts) ->
    {ok, Value};
required(_Args, _, _MapOpts, _Opts) ->
    {error, required_not_in_map}.

default(Default, _Value, #{is_key := false}, _Opts) ->
    {ok, default_value(Default)};
default(_Default, Value, #{is_key := true}, _Opts) ->
    {ok, Value};
default(_Args, _Value, #{}, _Options) ->
    {error, default_not_in_map}.

default_value([Default]) ->
    Default;
default_value(Default) ->
    Default.

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

'and'([_Validator|_T] = Validators, Value, MapOptions, Options) ->
    Options1 = 
        case maps:is_key(is_key, MapOptions) of
            true ->
                Options#{map_options => MapOptions};
            false ->
                Options
        end,
    and_1(Validators, Value, Options1);
'and'([], _Value, _MapOptions, _Options) ->
    {error, format_error}.

validator(Args, Value, Options) ->
    validator(Args, Value, #{}, Options).

validator([Function], Value, _MapOptions, _Options) when is_function(Function, 1) ->
    Function(Value);
validator([Function, Args], Value,  _MapOptions, _Options) when is_function(Function, 2), is_list(Args) ->
    Function(Args, Value);
validator([Function, Args], Value,  _MapOptions, Options) when is_function(Function, 3), is_list(Args) ->
    Function(Args, Value, Options);
validator([Function, Args], Value,  MapOptions, Options) when is_function(Function, 4), is_list(Args) ->
    Function(Args, Value, MapOptions, Options);
validator(_Args, _Value, _MapOptions, _Options) ->
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
        ok ->
            and_1(T, Value, Options);
        {ok, Value1} ->
            and_1(T, Value1, Options);
        {error, Reason} ->
            {error, Reason}
    end;
and_1([], Value, _Options) ->
    {ok, Value}.
