%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-07 19:48:42
%%%------------------------------------------------------------ 

-module(legolas_get_data_fsm).
-behaviour(gen_fsm).
-include("legolas.hrl").

%% API
-export([
         start_link/4,
         get_data/1
        ]).

%% Callbacks
-export([
         init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3
        ]).

%% States
-export([
         prepare/2,
         execute/2,
         waiting/2
        ]).

-record(state, 
        {
         req_id :: pos_integer(),
         from :: pid(),
         chash_n :: pos_integer(),
         chash_r :: pos_integer(),
         chash_w :: pos_integer(),
         path :: string(),
         preflist :: riak_core_apl:prefilist2(),
         num_r = 0 :: non_neg_integer(),
         num_error = 0 :: non_neg_integer(),
         replies = []
        }).

start_link(ReqId, From, CHashArgs, Path) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, CHashArgs, Path], []).

get_data(Path) ->
    ReqId = common_utils:random_id(),
    CHashArgs = legolas:get_chash_args(),
    legolas_get_data_fsm_sup:start_get_data_fsm([ReqId, self(), CHashArgs, Path]),
    {ok, ReqId}.

%%%------------------------------------------------------------ 
%%% Callbacks
%%%------------------------------------------------------------ 

init([ReqId, From, CHashArgs, Path]) ->
    {N, R, W} = CHashArgs,
    State = #state{
               req_id = ReqId,
               from = From,
               chash_n = N,
               chash_r = R,
               chash_w = W,
               path = Path
              },
    {ok, prepare, State, 0}.

prepare(timeout, State0=#state{
                           path = Path,
                           chash_n = N
                          }) ->

    Preflist = legolas:get_storage_preflist(Path, N),
    State = State0#state{preflist=Preflist},
    {next_state, execute, State, 0}.

execute(timeout, State0=#state{
                           req_id = ReqId,
                           path = Path,
                           preflist = Preflist
                          }) ->
    legolas_storage_vnode:get_data(Preflist, ReqId, Path),
    {next_state, waiting, State0}.

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
            State = State0#state{num_r = NumR},
            if 
                NumR =:= R ->
                    Reply =
                    case lists:any(common_utils:different(Data), Replies) of
                            true -> Replies;
                            false -> Data
                        end,
                    ?NOTICE("legolas get_data success read ~p times. Reply to ~p", [NumR, From]),
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






