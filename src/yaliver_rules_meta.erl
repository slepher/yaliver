%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(yaliver_rules_meta).

-include_lib("astranaut/include/astranaut.hrl").

%% API
-export([rules_meta/1]).

%%%===================================================================
%%% API
%%%===================================================================
rules_meta(Modules) ->
    {Modules1, []} = astranaut:ast_to_options(Modules),
    MetaMap = 
        lists:foldl(
          fun(Module, Acc1) ->
                  Exports = Module:module_info(exports),
                  lists:foldl(
                    fun({module_info, _}, Acc2) ->
                            Acc2;
                       ({Function, Arity}, Acc2) ->
                            maps:put(Function, {Module, Arity}, Acc2)
                    end, Acc1, Exports)
          end, maps:new(), Modules1),
    erl_syntax:revert(erl_syntax:abstract(MetaMap)).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
