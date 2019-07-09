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


-use_macro({yaliver_rules_meta, rules_meta/1, #{}}).

%%%===================================================================
%%% API
%%%===================================================================
validate(Schema, Object) ->
    validate(Schema, Object, #{}).

validate(Schema, Object, Options) when is_map(Schema), is_map(Object) ->
    validate({map, [Schema]}, Object, Options);
validate(Schema, Object, Options) when is_map(Options) ->
    validate_1(Schema, Object, Options);
validate(Schema, Object, Options) when is_list(Options) ->
    Options1 = proplists_to_map(Options),
    validate(Schema, Object, Options1).

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
validate_1(Validator, Object, #{root := true} = Options) when is_map(Object), is_map(Validator) ->
    validate_1({map, [Validator]}, Object, Options).
    
meta() ->
    yaliver_rules_meta:rules_meta([yaliver_base_rules, yaliver_string_rules, yaliver_collection_rules, yaliver_number_rules]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
apply_rule(RuleModule, ValidatorName, Arity, Args, Object, _Options) when is_list(Args) ->
    apply_rule_1(RuleModule, ValidatorName, Arity, Args, Object, _Options);
apply_rule(RuleModule, ValidatorName, Arity, Args, Object, _Options) ->
    apply_rule(RuleModule, ValidatorName, Arity, [Args], Object, _Options).

apply_rule_1(RuleModule, ValidatorName, 1, _Args, Object, _Options) ->
    RuleModule:ValidatorName(Object);
apply_rule_1(RuleModule, ValidatorName, 2, Args, Object, _Options) ->
    RuleModule:ValidatorName(Args, Object);
apply_rule_1(RuleModule, ValidatorName, 3, Args, Object, Options) ->
    RuleModule:ValidatorName(Args, Object, Options).




proplists_to_map(Proplists) ->
    proplists_to_map(Proplists, #{}).

proplists_to_map([{Key, Value}|T], Map) ->
    proplists_to_map(T, maps:put(Key, Value, Map));
proplists_to_map([Key|T], Map) when is_atom(Map) ->
    proplists_to_map(T, maps:put(Key, true, Map));
proplists_to_map([Other|_T], _Map) ->
    exit({invalid_options_value, Other}).
