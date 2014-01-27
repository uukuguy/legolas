%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas读取数据有限状态机。
%%%
%%% @end
%%% Created : 2013-11-07 19:48:42
%%%------------------------------------------------------------ 

-module(legolas_get_data_fsm).
-behaviour(gen_fsm).
-include("legolas.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
         start_link/4,
         get_data/1,
         get_data/2
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
         num_r = 0 :: non_neg_integer(),
         num_error = 0 :: non_neg_integer(),
         replies = []
        }).

%% ============================== APIs ==============================
%%

start_link(ReqId, From, Path, GetOptions) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Path, GetOptions], []).

%% ------------------------------ get_data ------------------------------ 
%% @doc 每次调用get_data，启动一个有限状态机。

get_data(Path) ->
    CHashArgs = legolas:get_chash_args(),
    GetOptions = CHashArgs,
    get_data(Path, GetOptions).

get_data(Path, GetOptions) ->
    ReqId = common_utils:random_id(),
    legolas_get_data_fsm_sup:start_get_data_fsm([ReqId, self(), Path, GetOptions]),
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
%% @doc 实际异步调用存储后端读取接口后，进入waiting状态

execute(timeout, State0=#state{
                           req_id = ReqId,
                           path = Path,
                           preflist = Preflist
                          }) ->
    legolas_storage_vnode:get_data(Preflist, ReqId, Path),
    {next_state, waiting, State0}.

%% ------------------------------ waiting state ------------------------------ 
%% @doc 根据实际调用成功次数要求，返回相应的值。

waiting({ok, ReqId, Result}, State0=#state{
                                       from = From,
                                       chash_n = N, 
                                       chash_r = R, 
                                       num_r = NumR0,
                                       num_error = NumError0,
                                       replies = Replies0
                                      }) ->
    case Result of
        {ok, Data} ->
            NumR = NumR0 + 1,
            Replies = [Data | Replies0],
            State = State0#state{num_r = NumR, replies = Replies},
            if 
                NumR =:= R ->
                    Reply =
                    case lists:any(common_utils:different(Data), Replies) of
                            true -> Replies;
                            false -> Data
                        end,
                    ?DEBUG("legolas get_data success read ~p times. Reply to ~p", [NumR, From]),
                    From ! {ReqId, ok, Reply},
                    {stop, normal, State};
                true -> {next_state, waiting, State}
            end;
        {error, _Reason} ->
            NumError = NumError0 + 1,
            State = State0#state{num_error = NumError},
            if
                NumError =:= N - R + 1 ->
                    ?ERROR("legolas get_data error read ~p times. Reply to ~p", [NumError, From]),
                    From ! {ReqId, error, "Read error num > 2"},
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






