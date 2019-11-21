%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(yaliver_collection_rules).

%% API
-export([map/3]).
-export([nested_object/3, variable_object/3]).
-export([list_of/3, list_of_objects/3, list_of_different_objects/3]).
%%%===================================================================
%%% API
%%%===================================================================
map([Args], Map, Options) when is_map(Map), is_map(Args) ->
    %% RestMap = maps:without(maps:keys(Args), Map),
    {Map1, Errors, RestOfMap} = 
        maps:fold(
          fun(Key, Validator, {MapAcc, ErrorAcc, RestAcc}) ->
                  {Value, IsKey, RestAcc1} = map_get_value(Key, RestAcc),
                  MapOptions = #{key => Key, parent => Map, is_key => IsKey},
                  Options1 = Options#{map_options => MapOptions},
                  case yaliver:validate_1(Validator, Value, Options1) of
                      {ok, undefined} ->
                          {MapAcc, ErrorAcc, RestAcc1};
                      {ok, Value1} ->
                          MapAcc1 = maps:put(Key, Value1, MapAcc),
                          {MapAcc1, ErrorAcc, RestAcc1};
                      {error, Reason} ->
                          {MapAcc, [{Key, Reason}|ErrorAcc], RestAcc1}
                  end
          end, {#{}, [], Map}, Args),
    case Errors of
        [] ->
            validate_rest_of_map(Map1, RestOfMap, Options);
        _ ->
            {error, Errors}
    end;
map(_Args, _Value, _Options) ->
    {error, format_error}.

validate_rest_of_map(Map, RestOfMap, #{strict := true}) ->
    case maps:keys(RestOfMap) of
        [] ->
            {ok, Map};
        Keys ->
            {error, {extra_keys, Keys}}
    end;
validate_rest_of_map(Map, RestOfMap, #{merge_rest := true}) ->
    Map1 = maps:merge(RestOfMap, Map),
    {ok, Map1};
validate_rest_of_map(Map, _RestOfMap, #{}) ->
    {ok, Map}.

nested_object(Args, Map, Options) ->
    map(Args, Map, Options).

variable_object([Key, ObjectArgs], Map, Options) ->
    {Value, IsKey, Rest} = map_get_value(Key, Map),
    case IsKey of
        true ->
            case map_get_value(Value, ObjectArgs) of
                {MapArgs, true, _} ->
                    case map([MapArgs], Rest, Options) of
                        {ok, Validated} ->
                            {ok, Validated#{Key => Value}};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {_, false, _} ->
                    {error, {unsupported_type, Value}}
            end;
        error ->
            {error, {no_type, Key}}
    end.

list_of([_Args], [], _Options) ->
    {ok, []};
list_of([Args], List, Options) when is_list(List) ->
    Options1 = maps:without([key, parent], Options),
    traversable:traverse(
      fun(A) ->
              yaliver:validate_1(Args, A, Options1)
      end, List);
list_of(Args, List, Options) when is_list(List) ->
    list_of([Args], List, Options);
list_of(_Args, _List, _Options) ->
    {error, invalid_format}.

list_of_objects([Args], List, Options) ->
    list_of([{map, [Args]}], List, Options);
list_of_objects(_Args, _List, _Options) ->
    {error, format_error}.

list_of_different_objects([Key, ObjectArgs], List, Options) when is_list(List) ->
    list_of([{variable_object, [Key, ObjectArgs]}], List, Options);
list_of_different_objects(_Args, _List, _Options) ->
    {error, format_error}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
map_get_value(Key, Map) when is_atom(Key) ->
    Keys = [Key, atom_to_binary(Key, utf8)],
    try_keys(Keys, Map);
map_get_value(Key, Map) when is_binary(Key) ->
    Keys = [Key, binary_to_atom(Key, utf8)],
    try_keys(Keys, Map).

try_keys([Key|T], Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Rest = maps:remove(Key, Map),
            {Value, true, Rest};
        error ->
            try_keys(T, Map)
    end;
try_keys([], Map) ->
    {undefined, false, Map}.
