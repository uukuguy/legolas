%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-07 14:30:12
%%%------------------------------------------------------------ 

-module(legolas_put_data_fsm).
-behaviour(gen_fsm).
-include("legolas.hrl").

%% APIs

-export([
         start_link/5,
         put_data/2
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
         data :: binary(),
         preflist :: riak_core_apl:prefilist2(),
         num_w = 0 :: non_neg_integer(),
         num_error = 0 :: non_neg_integer()
        }).

start_link(ReqId, From, CHashArgs, Path, Data) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, CHashArgs, Path, Data], []).

put_data(Path, Data) ->
    ReqId = common_utils:random_id(),
    CHashArgs = legolas:get_chash_args(),
    legolas_put_data_fsm_sup:start_put_data_fsm([ReqId, self(), CHashArgs, Path, Data]),
    {ok, ReqId}.

%%%------------------------------------------------------------ 
%%% Callbacks
%%%------------------------------------------------------------ 

init([ReqId, From, CHashArgs, Path, Data])->
    {N, R, W} = CHashArgs,
    State = #state{
               req_id = ReqId,
               from = From,
               chash_n = N,
               chash_r = R,
               chash_w = W,
               path = Path,
               data = Data
              },
    {ok, prepare, State, 0}.

prepare(timeout, State0=#state{
                           path = Path,
                           chash_n = N
                          }) ->
    Preflist = legolas:get_storage_preflist(Path, N),
    State = State0#state{preflist=Preflist},
    {next_state, execute, State, 0}.

execute(timeout, State=#state{
                           req_id = ReqId,
                           path = Path,
                           data = Data,
                           preflist = Preflist
                          }) ->
    legolas_storage_vnode:put_data(Preflist, ReqId, Path, Data),
    {next_state, waiting, State}.

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
                NumW =:= W ->
                    ?NOTICE("legolas put_data success write ~p times. Reply to ~p", [NumW, From]),
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




