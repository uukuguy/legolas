%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas写入数据有限状态机。
%%%
%%% @end
%%% Created : 2013-11-07 14:30:12
%%%------------------------------------------------------------ 

-module(legolas_put_data_fsm).
-behaviour(gen_fsm).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
         start_link/5,
         put_data/2,
         put_data/3
        ]).

%% ------------------------------ Callbacks ------------------------------ 
-export([
         init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3
        ]).

%% ------------------------------ States ------------------------------ 
-export([
         prepare/2,
         execute/2,
         waiting/2
        ]).

%% ------------------------------ record ------------------------------ 
-record(state, 
        {
         req_id :: pos_integer(),
         from :: pid(),
         chash_n :: pos_integer(),
         chash_r :: pos_integer(),
         chash_w :: pos_integer(),
         chash_dw :: pos_integer(),
         path :: string(),
         data :: binary(),
         preflist :: riak_core_apl:prefilist2(),
         num_w = 0 :: non_neg_integer(),
         num_error = 0 :: non_neg_integer()
        }).

%% ============================== APIs ==============================
%%

start_link(ReqId, From, Path, Data, PutOptions) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Path, Data, PutOptions], []).

%% ------------------------------ put_data ------------------------------ 
%% @doc 每次调用put_data，启动一个有限状态机。

put_data(Path, Data) ->
    CHashArgs = legolas:get_chash_args(),
    PutOptions = CHashArgs,
    put_data(Path, Data, PutOptions).

put_data(Path, Data, PutOptions) ->
    ReqId = common_utils:random_id(),
    legolas_put_data_fsm_sup:start_put_data_fsm([ReqId, self(), Path, Data, PutOptions]),
    {ok, ReqId}.

%% ============================== Callbacks ==============================
%% @doc init -> prepare - >execute -> waiting

%% ------------------------------ init ------------------------------ 
%% @doc 初始化完参数记录#state，进入prepare状态。
%%

init([ReqId, From, Path, Data, Options])->
    N = proplists:get_value(n, Options, ?DEFAULT_CHASH_N),
    R = proplists:get_value(r, Options, ?DEFAULT_CHASH_R),
    W = proplists:get_value(w, Options, ?DEFAULT_CHASH_W),
    DW = proplists:get_value(dw, Options, ?DEFAULT_CHASH_DW),
    State = #state{
               req_id = ReqId,
               from = From,
               chash_n = N,
               chash_r = R,
               chash_w = W,
               chash_dw = DW,
               path = Path,
               data = Data
              },
    {ok, prepare, State, 0}.

%% ------------------------------ prepare state ------------------------------ 
%% @doc 获取为路径Path提供存储服务的虚拟节点列表Preflist后，进入execute状态。
           
prepare(timeout, State0=#state{
                           path = Path,
                           chash_n = N
                          }) ->
    Preflist = legolas:get_storage_preflist(Path, N),
    State = State0#state{preflist=Preflist},
    {next_state, execute, State, 0}.

%% ------------------------------ execute state ------------------------------ 
%% @doc 实际异步调用存储后端写入接口后，进入waiting状态

execute(timeout, State=#state{
                           req_id = ReqId,
                           path = Path,
                           data = Data,
                           preflist = Preflist
                          }) ->
    legolas_storage_vnode:put_data(Preflist, ReqId, Path, Data),
    {next_state, waiting, State}.

%% ------------------------------ waiting state ------------------------------ 
%% @doc 根据实际调用成功次数要求，返回相应的值。

waiting({ok, ReqId, Result}, State0=#state{
                                       from = From, 
                                       chash_n = N, 
                                       chash_w = W, 
                                       num_w = NumW0, 
                                       num_error = NumError0
                                      }) ->
    case Result of
        ok ->
            NumW = NumW0 + 1,
            State = State0#state{num_w = NumW},
            if 
                %% 写入成功W(缺省2)次后，返回成功。
                NumW =:= W ->
                    ?DEBUG("legolas put_data success write ~p times. Reply to ~p", [NumW, From]),
                    From ! {ReqId, ok},
                    {stop, normal, State};
                true -> {next_state, waiting, State}
            end;
        {error, Reason} ->
            NumError = NumError0 + 1,
            State = State0#state{num_error = NumError},
            if
                NumError =:= N - W + 1 ->
                    From ! {ReqId, error, Reason},
                    {stop, normal, State};
                true -> {next_state, waiting, State}
            end
    end.

handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.




