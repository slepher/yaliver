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
    {Map1, Errors} = 
        maps:fold(
          fun(Key, Validator, {MapAcc, ErrorAcc}) ->
                  {Value, IsKey} = map_get_value(Key, Map),
                  Options1 =  Options#{key => Key, parent => Map, is_key => IsKey},
                  case yaliver:validate_1(Validator, Value, Options1) of
                      {ok, undefined} ->
                          {MapAcc, ErrorAcc};
                      {ok, Value1} ->
                          MapAcc1 = maps:put(Key, Value1, MapAcc),
                          {MapAcc1, ErrorAcc};
                      {error, Reason} ->
                          {MapAcc, [{Key, Reason}|ErrorAcc]}
                  end
          end, {#{}, []}, Args),
    case Errors of
        [] ->
            {ok, Map1};
        _ ->
            {error, Errors}
    end;
map(_Args, _Value, _Options) ->
    {error, format_error}.

nested_object(Args, Map, Options) ->
    map(Args, Map, Options).

variable_object([Key, ObjectArgs], Map, Options) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            case maps:find(Value, ObjectArgs) of
                {ok, MapArgs} ->
                    map([MapArgs#{Key => required}], Map, Options);
                error ->
                    {error, invalid_format}
            end;
        error ->
            {error, invalid_format}
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
            {Value, true};
        error ->
            try_keys(T, Map)
    end;
try_keys([], _Map) ->
    {undefined, false}.
