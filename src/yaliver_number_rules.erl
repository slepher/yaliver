%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(yaliver_number_rules).

-include_lib("erlando/include/do.hrl").

%% API
-export([integer/1]).
-export([positive_integer/1, negative_integer/1]).
%%%===================================================================
%%% API
%%%===================================================================
integer(Value) when is_integer(Value) ->
    {ok, Value};
integer(Value) when is_float(Value) ->
    {ok, round(Value)};
integer(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Integer ->
            {ok, Integer}
    catch
        _:_Exception ->
            {error, not_integer}
    end.

positive_integer(Integer) when is_integer(Integer) ->
    case Integer > 0 of
        true ->
            {ok, Integer};
        false ->
            {error, not_positive_integer}
    end;
positive_integer(Value) ->
    do([error_m ||
           Integer <- integer(Value),
           positive_integer(Integer)
       ]).

negative_integer(Integer) when is_integer(Integer) ->
    case Integer < 0 of
        true ->
            {ok, Integer};
        false ->
            {error, not_negative_integer}
    end;
negative_integer(Value) ->
    do([error_m ||
           Integer <- integer(Value),
           negative_integer(Integer)
       ]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
