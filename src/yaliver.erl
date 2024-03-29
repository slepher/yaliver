%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(yaliver).

-include_lib("astranaut/include/astranaut.hrl").

%% API
-export([validate/2, validate/3]).
-export([validate_1/3]).
-export([meta/0]).
-export([update_record/3]).

-use_macro({yaliver_rules_meta, rules_meta/1, #{}}).

%%%===================================================================
%%% API
%%%===================================================================
validate(Schema, Object) ->
    validate(Schema, Object, #{}).

validate(Schema, Object, Options) when is_map(Options) ->
    validate_1(Schema, Object, Options);
validate(Schema, Object, Options) when is_list(Options) ->
    Options1 = proplists_to_map(Options),
    validate(Schema, Object, Options1).

validate_1(Schema, Object, Options) when is_map(Schema), is_map(Object) ->
    validate({map, [Schema]}, Object, Options);
validate_1({ValidatorName, Args}, Object, Options) when is_atom(ValidatorName) ->
    ExtraRules = maps:get(extra_rules, Options, #{}),
    Rules = maps:merge(meta(), ExtraRules),
    case maps:find(ValidatorName, Rules) of
        {ok, {RuleModule, Arity}} ->
            apply_rule(RuleModule, ValidatorName, Arity, Args, Object, Options);
        error ->
            {error, {invalid_rule, ValidatorName}}
    end;
validate_1(ValidatorName, Object, Options) when is_atom(ValidatorName) ->
    validate_1({ValidatorName, []}, Object, Options);
validate_1(Validators, Object, Options) when is_list(Validators) ->
    validate_1({'and', Validators}, Object, Options);
validate_1(Validator, Object, Options) when is_function(Validator) ->
    {arity, Arity} = erlang:fun_info(Validator, arity),
    case apply_validator(Validator, Arity, Object, Options) of
        ok ->
            {ok, Object};
        {ok, V} ->
            {ok, V};
        {error, Reason} ->
            {error, Reason}
    end;

validate_1(Validator, Object, #{root := true} = Options) when is_map(Object), is_map(Validator) ->
    validate_1({map, [Validator]}, Object, Options).
    
meta() ->
    yaliver_rules_meta:rules_meta([yaliver_base_rules, yaliver_string_rules, yaliver_collection_rules, yaliver_number_rules]).

update_record(Record, Props, Format) ->
    maps:fold(
      fun(Key, Value, Acc) ->
              case maps:find(Key, Format) of
                  {ok, Element} ->
                      setelement(Element, Acc, Value);
                  error ->
                      Record
              end
      end, Record, Props).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
apply_rule(RuleModule, ValidatorName, Arity, Args, Object, Options) when is_list(Args) ->
    MapOptions = maps:get(map_options, Options, #{}),
    Options1 = maps:without([map_options], Options),
    apply_rule_1(RuleModule, ValidatorName, Arity, Args, Object, MapOptions, Options1);
apply_rule(RuleModule, ValidatorName, Arity, Args, Object, Options) ->
    apply_rule(RuleModule, ValidatorName, Arity, [Args], Object, Options).

apply_validator(Validator, Arity, Object, Options) ->
    MapOptions = maps:get(map_options, Options, #{}),
    Options1 = maps:without([map_options], Options),
    apply_validator(Validator, Arity, Object, MapOptions, Options1).

apply_validator(Validator, 3, Object, MapOptions, Options) ->
    Validator(Object, MapOptions, Options);
apply_validator(_Validator, _Arity, undefined, _MapOptions, _Options) ->
    {ok, undefined};
apply_validator(Validator, 2, Object, _MapOptions, Options) ->
    Validator(Object, Options);
apply_validator(Validator, 1, Object, _MapOptions, _Options) ->
    Validator(Object);
apply_validator(_Validator, _Arity, _Object, _MapOptions, _Options) ->
    {error, invalid_validator}.

apply_rule_1(RuleModule, ValidatorName, 4, Args, Object, MapOptions, Options) ->
    RuleModule:ValidatorName(Args, Object, MapOptions, Options);
apply_rule_1(_RuleModule, _ValidatorName, _Arity, _Args, undefined, _MapOptions, __Options) ->    {ok, undefined};
apply_rule_1(RuleModule, ValidatorName, 3, Args, Object, _MapOptions, Options) ->
    RuleModule:ValidatorName(Args, Object, Options);
apply_rule_1(RuleModule, ValidatorName, 2, Args, Object, _MapOptions,_Options) ->
    RuleModule:ValidatorName(Args, Object);
apply_rule_1(RuleModule, ValidatorName, 1, _Args, Object, _MapOptions, _Options) ->
    RuleModule:ValidatorName(Object).

proplists_to_map(Proplists) ->
    proplists_to_map(Proplists, #{}).

proplists_to_map([{Key, Value}|T], Map) ->
    proplists_to_map(T, maps:put(Key, Value, Map));
proplists_to_map([Key|T], Map) when is_atom(Key) ->
    proplists_to_map(T, maps:put(Key, true, Map));
proplists_to_map([], Acc) ->
    Acc;
proplists_to_map([Other|_T], _Map) ->
    exit({invalid_options_value, Other}).
