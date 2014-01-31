%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas删除数据有限状态机。
%%%
%%% @end
%%% Created : 2013-11-07 23:28:34
%%%------------------------------------------------------------ 

-module(legolas_delete_data_fsm).
-behaviour(gen_fsm).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
         start_link/4,
         delete_data/1,
         delete_data/2
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
         preflist :: riak_core_apl:prefilist2(),
         num_d = 0 :: non_neg_integer()
        }).

%% ============================== APIs ==============================
%%

start_link(ReqId, From, Path, DeleteOptions) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Path, DeleteOptions], []).

%% ------------------------------ delete_data ------------------------------ 
%% @doc 每次调用delete_data，启动一个有限状态机。

delete_data(Path) ->
    CHashArgs = legolas:get_chash_args(),
    DeleteOptions = CHashArgs,
    delete_data(Path, DeleteOptions).

delete_data(Path, DeleteOptions) ->
    ReqId = common_utils:random_id(),
    legolas_delete_data_fsm_sup:start_delete_data_fsm([ReqId, self(), Path, DeleteOptions]),
    {ok, ReqId}.

%% ============================== Callbacks ==============================
%% @doc init -> prepare - >execute -> waiting

%% ------------------------------ init ------------------------------ 
%% @doc 初始化完参数记录#state，进入prepare状态。
%%

init([ReqId, From, Path, Options]) ->
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
               path = Path
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
%% @doc 实际异步调用存储后端删除接口后，进入waiting状态

execute(timeout, State0=#state{
                           req_id = ReqId,
                           path = Path,
                           preflist = Preflist
                          }) ->
    legolas_storage_vnode:delete_data(Preflist, ReqId, Path),
    {next_state, waiting, State0}.

%% ------------------------------ waiting state ------------------------------ 
%% @doc 根据实际调用成功次数要求，返回相应的值。

waiting({ok, ReqId, Result}, State0=#state{
                                       from = From,
                                       chash_n = N, 
                                       num_d = NumD0
                                      }) ->
    case Result of
        ok ->
            NumD = NumD0 + 1,
            ?DEBUG("legolas delete_data has delete ~p times.", [NumD]),
            State = State0#state{num_d = NumD},
            if 
                NumD =:= N ->
                    ?DEBUG("legolas delete_data success delete ~p times. Reply to ~p", [NumD, From]),
                    From ! {ReqId, ok},
                    {stop, normal, State};
                true -> {next_state, waiting, State}
            end;
        {error, Reason} ->
            From ! {ReqId, error, Reason},
            {stop, normal, State0}
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

