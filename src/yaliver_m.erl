%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(yaliver_m).

-include_lib("erlando/include/erlando.hrl").
-include_lib("erlando/include/do.hrl").
-include_lib("erlando/include/gen_fun.hrl").

-erlando_type({?MODULE, []}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_reader).
-behaviour(monad_writer).

-compile({no_auto_import, [get/1, put/2]}).

%% API
%% -export(['>>='/3, '>>'/3, return/2]).

-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([ask/0, local/2, reader/1]).
-export([writer/1, tell/1, listen/1, pass/1]).

-gen_fun(#{patterns => [?MODULE], tbehaviours => [functor, applicative, monad, monad_reader, monad_writer]}).

%%%===================================================================
%%% API
%%%===================================================================
yaliver_m(Inner) ->
    {?MODULE, Inner}.

run_yaliver_m(#undetermined{} = UA) ->
    run_yaliver_m(undetermined:run(UA, ?MODULE));
run_yaliver_m({?MODULE, Inner}) ->
    Inner;
run_yaliver_m(Other) ->
    exit({invalid_yaliver, Other}).

fmap(F, YaliverA) ->
    RM = real_new(),
    map_real(
      fun(RealA) ->
              state_t:fmap(F, RealA, RM)
      end, YaliverA).

'<$'(YaliverA, YaliverB) ->
    functor:'default_<$'(YaliverA, YaliverB, ?MODULE).

pure(A) ->
    RM = real_new(),
    real_to_yaliver_m(applicative:pure(A, RM)).

'<*>'(YaliverA, YaliverB) ->
    RealM = real_new(),
    RealA = yaliver_m_to_real(YaliverA),
    RealB = yaliver_m_to_real(YaliverB),
    real_to_yaliver_m(applicative:'<*>'(RealA, RealB, RealM)).

lift_a2(F, YaliverA, YaliverB) ->
    applicative:default_lift_a2(F, YaliverA, YaliverB, ?MODULE).

'*>'(YaliverA, YaliverB) ->
    applicative:'default_*>'(YaliverA, YaliverB, ?MODULE).

'<*'(YaliverA, YaliverB) ->
    applicative:'default_<*'(YaliverA, YaliverB, ?MODULE).

'>>='(YaliverA, KYaliverB) ->
    RM = real_new(),
    RealA = yaliver_m_to_real(YaliverA),
    KRealB =  fun(A) -> yaliver_m_to_real(KYaliverB(A)) end,
    real_to_yaliver_m(monad:'>>='(RealA, KRealB, RM)).

'>>'(YaliverA, YaliverB) ->
    monad:'default_>>'(YaliverA, YaliverB, ?MODULE).

return(A) ->
    monad:default_return(A, ?MODULE).

ask() ->
    RM = real_new(),
    real_to_yaliver_m(monad_reader:ask(RM)).

local(F, YaliverA) ->
    RM = real_new(),
    map_real(
      fun(RealA) ->
              monad_reader:local(F, RealA, RM)
      end, YaliverA).
    
reader(F) ->
    RM = real_new(),
    real_to_yaliver_m(monad_reader:reader(F, RM)).

writer({A, W}) ->
    RM = real_new(),
    real_to_yaliver_m(monad_writer:writer({A, W}, RM)).

tell(W) ->
    RM = real_new(),
    real_to_yaliver_m(monad_writer:tell(W, RM)).

listen(YaliverA) ->
    RM = real_new(),
    real_to_yaliver_m(monad_writer:listen(yaliver_m_to_real(YaliverA), RM)).

pass(YaliverAF) ->
    RM = real_new(),
    real_to_yaliver_m(monad_writer:listen(yaliver_m_to_real(YaliverAF), RM)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
real_new() ->
    writer_t:new(reader_t:new(identity)).

yaliver_m_to_real(YaliverM) ->
    writer_t:writer_t(run_yaliver_m(YaliverM)).

real_to_yaliver_m(WriterT) ->
    yaliver_m(writer_t:run_writer_t(WriterT)).

map_real(F, YaliverA) ->
    real_to_yaliver_m(F(yaliver_m_to_real(YaliverA))).
