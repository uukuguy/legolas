%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-07 23:28:34
%%%------------------------------------------------------------ 

-module(legolas_delete_data_fsm).
-behaviour(gen_fsm).
-include("legolas.hrl").

%% API
-export([
         start_link/4,
         delete_data/1,
         delete_data/2
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
         chash_dw :: pos_integer(),
         path :: string(),
         preflist :: riak_core_apl:prefilist2(),
         num_d = 0 :: non_neg_integer()
        }).

start_link(ReqId, From, Path, DeleteOptions) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Path, DeleteOptions], []).

delete_data(Path) ->
    CHashArgs = legolas:get_chash_args(),
    DeleteOptions = CHashArgs,
    delete_data(Path, DeleteOptions).

delete_data(Path, DeleteOptions) ->
    ReqId = common_utils:random_id(),
    legolas_delete_data_fsm_sup:start_delete_data_fsm([ReqId, self(), Path, DeleteOptions]),
    {ok, ReqId}.

%%%------------------------------------------------------------ 
%%% Callbacks
%%%------------------------------------------------------------ 

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
    legolas_storage_vnode:delete_data(Preflist, ReqId, Path),
    {next_state, waiting, State0}.

waiting({ok, ReqId, Result}, State0=#state{
                                       from = From,
                                       chash_n = N, 
                                       num_d = NumD0
                                      }) ->
    case Result of
        ok ->
            NumD = NumD0 + 1,
            ?NOTICE("legolas delete_data has delete ~p times.", [NumD]),
            State = State0#state{num_d = NumD},
            if 
                NumD =:= N ->
                    ?NOTICE("legolas delete_data success delete ~p times. Reply to ~p", [NumD, From]),
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






