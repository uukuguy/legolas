%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas交换有限状态机。
%%%
%%% @end
%%% Created : 2014-01-15 02:12:31
%%%------------------------------------------------------------ 
-module(legolas_exchange_fsm).
-behaviour(gen_fsm).

%% ------------------------------ APIs ------------------------------ 
-export([
         start/5
        ]).

%% ------------------------------ Callbacks ------------------------------ 
-export([
         init/1, 
         handle_event/3, 
         handle_sync_event/4, 
         handle_info/3,
         terminate/3, 
         code_change/4
        ]).

%% ------------------------------ States ------------------------------ 
-export([
         prepare_exchange/2,
         update_trees/2,
         key_exchange/2
        ]).

%% ------------------------------ record ------------------------------ 
-type index() :: non_neg_integer().
-type index_n() :: {index(), pos_integer()}.
-type vnode() :: {index(), node()}.

-record(state, {local       :: vnode(),
                remote      :: vnode(),
                index_n     :: index_n(),
                local_tree  :: pid(),
                remote_tree :: pid(),
                built       :: non_neg_integer(),
                timeout     :: pos_integer()
               }).

%% Per state transition timeout used by certain transitions
-define(DEFAULT_ACTION_TIMEOUT, 300000). %% 5 minutes

%% ============================== APIs ==============================
%%

start(LocalVN, RemoteVN, IndexN, Tree, Manager) ->
    gen_fsm:start(?MODULE, [LocalVN, RemoteVN, IndexN, Tree, Manager], []).

%% ============================== Callbacks ==============================
%% @doc init -> prepare_exchange -> update_trees -> key_exchange

%% ------------------------------ init ------------------------------ 
init([LocalVN, RemoteVN, IndexN, LocalTree, Manager]) ->
    Timeout = app_helper:get_env(legolas,
                                 anti_entropy_timeout,
                                 ?DEFAULT_ACTION_TIMEOUT),
    monitor(process, Manager),
    monitor(process, LocalTree),
    State = #state{local=LocalVN,
                   remote=RemoteVN,
                   index_n=IndexN,
                   local_tree=LocalTree,
                   timeout=Timeout,
                   built=0},
    gen_fsm:send_event(self(), start_exchange),
    lager:debug("Starting exchange: ~p", [LocalVN]),
    {ok, prepare_exchange, State}.

%% ------------------------------ prepare_exchange state ------------------------------ 
%% @doc Initial state. Attempt to acquire all necessary exchange locks.
%%      In order, acquire local concurrency lock, local tree lock,
%%      remote concurrency lock, and remote tree lock. Exchange will
%%      timeout if locks cannot be acquired in a timely manner.
prepare_exchange(start_exchange, State=#state{remote=RemoteVN,
                                              index_n=IndexN}) ->
    case legolas_entropy_manager:get_lock(exchange) of
        ok ->
            case legolas_index_hashtree:get_lock(State#state.local_tree,
                                                 local_fsm) of
                ok ->
                    remote_exchange_request(RemoteVN, IndexN),
                    next_state_with_timeout(prepare_exchange, State);
                _ ->
                    send_exchange_status(already_locked, State),
                    {stop, normal, State}
            end;
        Error ->
            send_exchange_status(Error, State),
            {stop, normal, State}
    end;
prepare_exchange(timeout, State) ->
    do_timeout(State);
prepare_exchange({remote_exchange, Pid}, State) when is_pid(Pid) ->
    monitor(process, Pid),
    State2 = State#state{remote_tree=Pid},
    update_trees(start_exchange, State2);
prepare_exchange({remote_exchange, Error}, State) ->
    send_exchange_status({remote, Error}, State),
    {stop, normal, State}.

%% ~~~~~~~~~~~~~~~~~~~~ remote_exchange_request ~~~~~~~~~~~~~~~~~~~~
remote_exchange_request(RemoteVN, IndexN) ->
    FsmPid = self(),
    as_event(fun() ->
                     legolas_entropy_manager:start_exchange_remote(RemoteVN,
                                                                   IndexN,
                                                                   FsmPid)
             end).

%% ~~~~~~~~~~~~~~~~~~~~ next_state_with_timeout ~~~~~~~~~~~~~~~~~~~~
next_state_with_timeout(StateName, State) ->
    next_state_with_timeout(StateName, State, State#state.timeout).
next_state_with_timeout(StateName, State, Timeout) ->
    {next_state, StateName, State, Timeout}.

%% ~~~~~~~~~~~~~~~~~~~~ do_timeout ~~~~~~~~~~~~~~~~~~~~
do_timeout(State=#state{local=LocalVN,
                        remote=RemoteVN,
                        index_n=IndexN}) ->
    lager:info("Timeout during exchange between (local) ~p and (remote) ~p, "
               "(preflist) ~p", [LocalVN, RemoteVN, IndexN]),
    send_exchange_status({timeout, RemoteVN, IndexN}, State),
    {stop, normal, State}.

%% ------------------------------ update_trees state ------------------------------ 
%% @doc Now that locks have been acquired, ask both the local and remote
%%      hashtrees to perform a tree update. If updates do not occur within
%%      a timely manner, the exchange will timeout. Since the trees will
%%      continue to finish the update even after the exchange times out,
%%      a future exchange should eventually make progress.
update_trees(start_exchange, State=#state{local=LocalVN,
                                          remote=RemoteVN,
                                          local_tree=LocalTree,
                                          remote_tree=RemoteTree,
                                          index_n=IndexN}) ->
    lager:debug("Sending to ~p", [LocalVN]),
    lager:debug("Sending to ~p", [RemoteVN]),

    update_request(LocalTree, LocalVN, IndexN),
    update_request(RemoteTree, RemoteVN, IndexN),
    {next_state, update_trees, State};

update_trees({not_responsible, VNodeIdx, IndexN}, State) ->
    lager:debug("VNode ~p does not cover preflist ~p", [VNodeIdx, IndexN]),
    send_exchange_status({not_responsible, VNodeIdx, IndexN}, State),
    {stop, normal, State};
update_trees({tree_built, _, _}, State) ->
    Built = State#state.built + 1,
    case Built of
        2 ->
            lager:debug("Moving to key exchange"),
            {next_state, key_exchange, State, 0};
        _ ->
            {next_state, update_trees, State#state{built=Built}}
    end.

%% ~~~~~~~~~~~~~~~~~~~~ update_request ~~~~~~~~~~~~~~~~~~~~
update_request(Tree, {Index, _}, IndexN) ->
    as_event(fun() ->
                     case legolas_index_hashtree:update(IndexN, Tree) of
                         ok ->
                             {tree_built, Index, IndexN};
                         not_responsible ->
                             {not_responsible, Index, IndexN}
                     end
             end).

%% ------------------------------ key_exchange state ------------------------------ 
%% @doc Now that locks have been acquired and both hashtrees have been updated,
%%      perform a key exchange and trigger read repair for any divergent keys.
key_exchange(timeout, State=#state{local=LocalVN,
                                   remote=RemoteVN,
                                   local_tree=LocalTree,
                                   remote_tree=RemoteTree,
                                   index_n=IndexN}) ->
    lager:debug("Starting key exchange between ~p and ~p", [LocalVN, RemoteVN]),
    lager:debug("Exchanging hashes for preflist ~p", [IndexN]),

    Remote = fun(get_bucket, {L, B}) ->
                     exchange_bucket(RemoteTree, IndexN, L, B);
                (key_hashes, Segment) ->
                     exchange_segment(RemoteTree, IndexN, Segment);
                (_, _) ->
                     ok
             end,

    %% Unclear if we should allow exchange to run indefinitely or enforce
    %% a timeout. The problem is that depending on the number of keys and
    %% key differences, exchange can take arbitrarily long. For now, go with
    %% unbounded exchange, with the ability to cancel exchanges through the
    %% entropy manager if needed.
    {ok, RC} = riak:local_client(),
    AccFun = fun(KeyDiff, Acc) ->
                     lists:foldl(fun(Diff, Acc2) ->
                                         read_repair_keydiff(RC, LocalVN, RemoteVN, Diff),
                                         case Acc2 of
                                             [] ->
                                                 [1];
                                             [Count] ->
                                                 [Count+1]
                                         end
                                 end, Acc, KeyDiff)
             end,
    %% TODO: Add stats for AAE
    case legolas_index_hashtree:compare(IndexN, Remote, AccFun, LocalTree) of
        [] ->
            exchange_complete(LocalVN, RemoteVN, IndexN, 0),
            ok;
        [Count] ->
            exchange_complete(LocalVN, RemoteVN, IndexN, Count),
            lager:info("Repaired ~b keys during active anti-entropy exchange "
                       "of ~p between ~p and ~p",
                       [Count, IndexN, LocalVN, RemoteVN])
    end,
    {stop, normal, State}.

%% ~~~~~~~~~~~~~~~~~~~~ exchange_bucket ~~~~~~~~~~~~~~~~~~~~
exchange_bucket(Tree, IndexN, Level, Bucket) ->
    legolas_index_hashtree:exchange_bucket(IndexN, Level, Bucket, Tree).

%% ~~~~~~~~~~~~~~~~~~~~ exchange_segment ~~~~~~~~~~~~~~~~~~~~
exchange_segment(Tree, IndexN, Segment) ->
    legolas_index_hashtree:exchange_segment(IndexN, Segment, Tree).

%% ~~~~~~~~~~~~~~~~~~~~ read_repair_keydiff ~~~~~~~~~~~~~~~~~~~~
read_repair_keydiff(RC, LocalVN, RemoteVN, {_, KeyBin}) ->
    {Bucket, Key} = binary_to_term(KeyBin),
    %% TODO: Even though this is at debug level, it's still extremely
    %%       spammy. Should this just be removed? We can always use
    %%       redbug to trace read_repair_keydiff when needed. Of course,
    %%       users can't do that.
    %% lager:debug("Anti-entropy forced read repair: ~p/~p", [Bucket, Key]),
    RC:get(Bucket, Key),
    %% Force vnodes to update AAE tree in case read repair wasn't triggered
    legolas_vnode:rehash([LocalVN, RemoteVN], Bucket, Key),
    ok.

%% ~~~~~~~~~~~~~~~~~~~~ exchange_complete ~~~~~~~~~~~~~~~~~~~~
exchange_complete({LocalIdx, _}, {RemoteIdx, RemoteNode}, IndexN, Repaired) ->
    legolas_entropy_info:exchange_complete(LocalIdx, RemoteIdx, IndexN, Repaired),
    rpc:call(RemoteNode, legolas_entropy_info, exchange_complete,
             [RemoteIdx, LocalIdx, IndexN, Repaired]).

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({'DOWN', _, _, _, _}, _StateName, State) ->
    %% Either the entropy manager, local hashtree, or remote hashtree has
    %% exited. Stop exchange.
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ============================== Internal functions ==============================
%%

%% ------------------------------ as_event ------------------------------ 
as_event(F) ->
    Self = self(),
    spawn_link(fun() ->
                       Result = F(),
                       gen_fsm:send_event(Self, Result)
               end),
    ok.

%% ------------------------------ send_exchange_status ------------------------------ 
send_exchange_status(Status, #state{local=LocalVN,
                                    remote=RemoteVN,
                                    index_n=IndexN}) ->
    legolas_entropy_manager:exchange_status(LocalVN, RemoteVN, IndexN, Status).

