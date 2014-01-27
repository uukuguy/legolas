%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas熵管理器
%%%
%%% @end
%%% Created : 2013-11-30 09:21:35
%%%------------------------------------------------------------ 

-module(legolas_entropy_manager).
-behaviour(gen_server).

%% ------------------------------ APIs ------------------------------ 
-export([
         start_link/0,
         get_lock/1,
         get_lock/2,
         start_exchange_remote/3,
         requeue_poke/1,
         exchange_status/4,
         enabled/0,
         set_mode/1,
         set_debug/1,
         enable/0,
         disable/0,
         manual_exchange/1,
         cancel_exchange/1,
         cancel_exchanges/0
       ]).

-export([
         all_pairwise_exchanges/2
        ]).

%% ------------------------------ Callbacks ------------------------------ 
%% gen_server callbacks
-export([
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
        ]).

%% ------------------------------ record ------------------------------ 
-type index() :: non_neg_integer().
-type index_n() :: {index(), pos_integer()}.
-type vnode() :: {index(), node()}.
-type exchange() :: {index(), index(), index_n()}.
-type riak_core_ring() :: riak_core_ring:riak_core_ring().

-record(state, {mode           = automatic :: automatic | manual,
                trees          = []        :: [{index(), pid()}],
                tree_queue     = []        :: [{index(), pid()}],
                locks          = []        :: [{pid(), reference()}],
                build_tokens   = 0         :: non_neg_integer(),
                exchange_queue = []        :: [exchange()],
                exchanges      = []        :: [{index(), reference(), pid()}]
               }).

-type state() :: #state{}.

-define(DEFAULT_CONCURRENCY, 2).
-define(DEFAULT_BUILD_LIMIT, {1, 3600000}). %% Once per hour

%% ============================== APIs ==============================
%%

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------ get_lock ------------------------------ 
%% @doc Acquire an exchange concurrency lock if available, and associate
%%      the lock with the calling process.
-spec get_lock(any()) -> ok | max_concurrency.
get_lock(Type) ->
    get_lock(Type, self()).

%% @doc Acquire an exchange concurrency lock if available, and associate
%%      the lock with the provided pid.
-spec get_lock(any(), pid()) -> ok | max_concurrency.
get_lock(Type, Pid) ->
    gen_server:call(?MODULE, {get_lock, Type, Pid}, infinity).


%% ------------------------------ start_exchange_remote ------------------------------ 
%% @doc Acquire the necessary locks for an entropy exchange with the specified
%%      remote vnode. The request is sent to the remote entropy manager which
%%      will try to acquire a concurrency lock. If successsful, the request is
%%      then forwarded to the relevant index_hashtree to acquire a tree lock.
%%      If both locks are acquired, the pid of the remote index_hashtree is
%%      returned.
-spec start_exchange_remote({index(), node()}, index_n(), pid())
                           -> {remote_exchange, pid()} |
                              {remote_exchange, anti_entropy_disabled} |
                              {remote_exchange, max_concurrency} |
                              {remote_exchange, not_built} |
                              {remote_exchange, already_locked}.
start_exchange_remote(_VNode={Index, Node}, IndexN, FsmPid) ->
    gen_server:call({?MODULE, Node},
                    {start_exchange_remote, FsmPid, Index, IndexN},
                    infinity).

%% ------------------------------ requeue_poke ------------------------------ 
%% @doc Used by {@link legolas_index_hashtree} to requeue a poke on
%%      build failure.
-spec requeue_poke(index()) -> ok.
requeue_poke(Index) ->
    gen_server:cast(?MODULE, {requeue_poke, Index}).

%% ------------------------------ exchange_status ------------------------------ 
%% @doc Used by {@link legolas_exchange_fsm} to inform the entropy
%%      manager about the status of an exchange (ie. completed without
%%      issue, failed, etc)
-spec exchange_status(vnode(), vnode(), index_n(), any()) -> ok.
exchange_status(LocalVN, RemoteVN, IndexN, Reply) ->
    gen_server:cast(?MODULE,
                    {exchange_status,
                     self(), LocalVN, RemoteVN, IndexN, Reply}).

%% ------------------------------ enabled ------------------------------ 
%% @doc Returns true of AAE is enabled, false otherwise.
-spec enabled() -> boolean().
enabled() ->
    {Enabled, _} = settings(),
    Enabled.

%% ------------------------------ set_mode ------------------------------ 
%% @doc Set AAE to either `automatic' or `manual' mode. In automatic mode, the
%%      entropy manager triggers all necessary hashtree exchanges. In manual
%%      mode, exchanges must be triggered using {@link manual_exchange/1}.
%%      Regardless of exchange mode, the entropy manager will always ensure
%%      local hashtrees are built and rebuilt as necessary.
-spec set_mode(automatic | manual) -> ok.
set_mode(Mode=automatic) ->
    ok = gen_server:call(?MODULE, {set_mode, Mode}, infinity);
set_mode(Mode=manual) ->
    ok = gen_server:call(?MODULE, {set_mode, Mode}, infinity).

%% ------------------------------ set_debug ------------------------------ 
%% @doc Toggle debug mode, which prints verbose AAE information to the console.
-spec set_debug(boolean()) -> ok.
set_debug(Enabled) ->
    Modules = [legolas_index_hashtree,
               legolas_entropy_manager,
               legolas_exchange_fsm],
    case Enabled of
        true ->
            [lager:trace_console([{module, Mod}]) || Mod <- Modules];
        false ->
            [begin
                 {ok, Trace} = lager:trace_console([{module, Mod}]),
                 lager:stop_trace(Trace)
             end || Mod <- Modules]
    end,
    ok.

%% ------------------------------ enable ------------------------------ 
enable() ->
    gen_server:call(?MODULE, enable, infinity).

%% ------------------------------ disable ------------------------------ 
disable() ->
    gen_server:call(?MODULE, disable, infinity).

%% ------------------------------ manual_exchange ------------------------------ 
%% @doc Manually trigger hashtree exchanges.
%%      -- If an index is provided, trigger exchanges between the index and all
%%         sibling indices for all index_n.
%%      -- If both an index and index_n are provided, trigger exchanges between
%%         the index and all sibling indices associated with the specified
%%         index_n.
%%      -- If an index, remote index, and index_n are provided, trigger an
%%         exchange between the index and remote index for the specified
%%         index_n.
-spec manual_exchange(index() |
                      {index(), index_n()} |
                      {index(), index(), index_n()}) -> ok.
manual_exchange(Exchange) ->
    gen_server:call(?MODULE, {manual_exchange, Exchange}, infinity).

%% ------------------------------ cancel_exchange ------------------------------ 
-spec cancel_exchange(index()) -> ok | undefined.
cancel_exchange(Index) ->
    gen_server:call(?MODULE, {cancel_exchange, Index}, infinity).

%% ------------------------------ cancel_exchanges ------------------------------ 
-spec cancel_exchanges() -> [index()].
cancel_exchanges() ->
    gen_server:call(?MODULE, cancel_exchanges, infinity).


%% ============================== Callbacks ==============================
%%

%% ------------------------------ init ------------------------------ 
%% @doc 
-spec init([]) -> {'ok',state()}.
init([]) ->
    schedule_tick(),
    {_, Opts} = settings(),
    Mode = case proplists:is_defined(manual, Opts) of
               true ->
                   manual;
               false ->
                   automatic
           end,
    set_debug(proplists:is_defined(debug, Opts)),
    State = #state{mode=Mode,
                   trees=[],
                   tree_queue=[],
                   locks=[],
                   exchanges=[],
                   exchange_queue=[]},
    State2 = reset_build_tokens(State),
    schedule_reset_build_tokens(),
    {ok, State2}.

%% ------------------------------ handle_call ------------------------------ 
handle_call({set_mode, Mode}, _From, State) ->
    {reply, ok, State#state{mode=Mode}};
handle_call({manual_exchange, Exchange}, _From, State) ->
    State2 = enqueue_exchange(Exchange, State),
    {reply, ok, State2};
handle_call(enable, _From, State) ->
    {_, Opts} = settings(),
    application:set_env(legolas, anti_entropy, {on, Opts}),
    {reply, ok, State};
handle_call(disable, _From, State) ->
    {_, Opts} = settings(),
    application:set_env(legolas, anti_entropy, {off, Opts}),
    [legolas_index_hashtree:stop(T) || {_,T} <- State#state.trees],
    {reply, ok, State};
handle_call({get_lock, Type, Pid}, _From, State) ->
    {Reply, State2} = do_get_lock(Type, Pid, State),
    {reply, Reply, State2};
handle_call({start_exchange_remote, FsmPid, Index, IndexN}, From, State) ->
    case {enabled(),
          orddict:find(Index, State#state.trees)} of
        {false, _} ->
            {reply, {remote_exchange, anti_entropy_disabled}, State};
        {_, error} ->
            {reply, {remote_exchange, not_built}, State};
        {_, {ok, Tree}} ->
            case do_get_lock(exchange_remote, FsmPid, State) of
                {ok, State2} ->
                    %% Concurrency lock acquired, now forward to index_hashtree
                    %% to acquire tree lock.
                    legolas_index_hashtree:start_exchange_remote(FsmPid, From, IndexN, Tree),
                    {noreply, State2};
                {Reply, State2} ->
                    {reply, {remote_exchange, Reply}, State2}
            end
    end;
handle_call({cancel_exchange, Index}, _From, State) ->
    case lists:keyfind(Index, 1, State#state.exchanges) of
        false ->
            {reply, undefined, State};
        {Index, _Ref, Pid} ->
            exit(Pid, kill),
            {reply, ok, State}
    end;
handle_call(cancel_exchanges, _From, State=#state{exchanges=Exchanges}) ->
    Indices = [begin
                   exit(Pid, kill),
                   Index
               end || {Index, _Ref, Pid} <- Exchanges],
    {reply, Indices, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.



%% ~~~~~~~~~~~~~~~~~~~~ do_get_lock ~~~~~~~~~~~~~~~~~~~~
-spec do_get_lock(any(),pid(),state())
                 -> {ok | max_concurrency | build_limit_reached, state()}.
do_get_lock(Type, Pid, State=#state{locks=Locks}) ->
    Concurrency = app_helper:get_env(legolas,
                                     anti_entropy_concurrency,
                                     ?DEFAULT_CONCURRENCY),
    case length(Locks) >= Concurrency of
        true ->
            {max_concurrency, State};
        false ->
            case check_lock_type(Type, State) of
                {ok, State2} ->
                    Ref = monitor(process, Pid),
                    State3 = State2#state{locks=[{Pid,Ref}|Locks]},
                    {ok, State3};
                Error ->
                    {Error, State}
            end
    end.

%% ~~~~~~~~~~~~~~~~~~~~ check_lock_type ~~~~~~~~~~~~~~~~~~~~
check_lock_type(build, State=#state{build_tokens=Tokens}) ->
    if Tokens > 0 ->
            {ok, State#state{build_tokens=Tokens-1}};
       true ->
            build_limit_reached
    end;
check_lock_type(_Type, State) ->
    {ok, State}.

%% ------------------------------ handle_cast ------------------------------ 
handle_cast({requeue_poke, Index}, State) ->
    State2 = requeue_poke(Index, State),
    {noreply, State2};
handle_cast({exchange_status, Pid, LocalVN, RemoteVN, IndexN, Reply}, State) ->
    State2 = do_exchange_status(Pid, LocalVN, RemoteVN, IndexN, Reply, State),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------ handle_info ------------------------------ 
handle_info(tick, State) ->
    State2 = maybe_tick(State),
    {noreply, State2};
handle_info(reset_build_tokens, State) ->
    State2 = reset_build_tokens(State),
    schedule_reset_build_tokens(),
    {noreply, State2};
handle_info({{hashtree_pid, Index}, Reply}, State) ->
    case Reply of
        {ok, Pid} when is_pid(Pid) ->
            State2 = add_hashtree_pid(Index, Pid, State),
            {noreply, State2};
        _ ->
            {noreply, State}
    end;
handle_info({'DOWN', Ref, _, Obj, Status}, State) ->
    State2 = maybe_release_lock(Ref, State),
    State3 = maybe_clear_exchange(Ref, Status, State2),
    State4 = maybe_clear_registered_tree(Obj, State3),
    {noreply, State4};
handle_info(_Info, State) ->
    {noreply, State}.

%% ~~~~~~~~~~~~~~~~~~~~ maybe_tick ~~~~~~~~~~~~~~~~~~~~
-spec maybe_tick(state()) -> state().
maybe_tick(State) ->
    case enabled() of
        true ->
            case riak_core_capability:get({legolas, anti_entropy}, disabled) of
                disabled ->
                    NextState = State;
                enabled_v1 ->
                    NextState = tick(State)
            end;
        false ->
            %% Ensure we do not have any running index_hashtrees, which can
            %% happen when disabling anti-entropy on a live system.
            [legolas_index_hashtree:stop(T) || {_,T} <- State#state.trees],
            NextState = State
    end,
    schedule_tick(),
    NextState.

%% ~~~~~~~~~~~~~~~~~~~~ myabe_release_lock ~~~~~~~~~~~~~~~~~~~~
-spec maybe_release_lock(reference(), state()) -> state().
maybe_release_lock(Ref, State) ->
    Locks = lists:keydelete(Ref, 2, State#state.locks),
    State#state{locks=Locks}.

%% ~~~~~~~~~~~~~~~~~~~~ myabe_clear_exchange ~~~~~~~~~~~~~~~~~~~~
-spec maybe_clear_exchange(reference(), term(), state()) -> state().
maybe_clear_exchange(Ref, Status, State) ->
    case lists:keytake(Ref, 2, State#state.exchanges) of
        false ->
            State;
        {value, {Idx,Ref,_Pid}, Exchanges} ->
            lager:debug("Untracking exchange: ~p :: ~p", [Idx, Status]),
            State#state{exchanges=Exchanges}
    end.

%% ~~~~~~~~~~~~~~~~~~~~ myabe_clear_registered_tree ~~~~~~~~~~~~~~~~~~~~
-spec maybe_clear_registered_tree(pid(), state()) -> state().
maybe_clear_registered_tree(Pid, State) when is_pid(Pid) ->
    Trees = lists:keydelete(Pid, 2, State#state.trees),
    State#state{trees=Trees};
maybe_clear_registered_tree(_, State) ->
    State.

%% ~~~~~~~~~~~~~~~~~~~~ add_hashtree_pid ~~~~~~~~~~~~~~~~~~~~
add_hashtree_pid(Index, Pid, State) ->
    add_hashtree_pid(enabled(), Index, Pid, State).

add_hashtree_pid(false, _Index, Pid, State) ->
    legolas_index_hashtree:stop(Pid),
    State;
add_hashtree_pid(true, Index, Pid, State=#state{trees=Trees}) ->
    case orddict:find(Index, Trees) of
        {ok, Pid} ->
            %% Already know about this hashtree
            State;
        _ ->
            monitor(process, Pid),
            Trees2 = orddict:store(Index, Pid, Trees),
            State2 = State#state{trees=Trees2},
            State3 = add_index_exchanges(Index, State2),
            State3
    end.

%% ------------------------------ terminate ------------------------------ 
terminate(_Reason, _State) ->
    ok.

%% ------------------------------ code_change ------------------------------ 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================== Internal functions ==============================
%%

%% ------------------------------ schedule_reset_build_tokens ------------------------------ 
schedule_reset_build_tokens() ->
    {_, Reset} = app_helper:get_env(legolas, anti_entropy_build_limit,
                                    ?DEFAULT_BUILD_LIMIT),
    erlang:send_after(Reset, self(), reset_build_tokens).

%% ------------------------------ reset_build_tokens ------------------------------ 
reset_build_tokens(State) ->
    {Tokens, _} = app_helper:get_env(legolas, anti_entropy_build_limit,
                                     ?DEFAULT_BUILD_LIMIT),
    State#state{build_tokens=Tokens}.

%% ------------------------------ settings ------------------------------ 
-spec settings() -> {boolean(), proplists:proplist()}.
settings() ->
    case app_helper:get_env(legolas, anti_entropy, {off, []}) of
        {on, Opts} ->
            {true, Opts};
        {off, Opts} ->
            {false, Opts};
        X ->
            lager:warning("Invalid setting for legolas/anti_entropy: ~p", [X]),
            application:set_env(legolas, anti_entropy, {off, []}),
            {false, []}
    end.

%% ------------------------------ schedule_tick ------------------------------ 
-spec schedule_tick() -> ok.
schedule_tick() ->
    %% Perform tick every 15 seconds
    DefaultTick = 15000,
    Tick = app_helper:get_env(legolas,
                              anti_entropy_tick,
                              DefaultTick),
    erlang:send_after(Tick, ?MODULE, tick),
    ok.

%% ------------------------------ tick ------------------------------ 
-spec tick(state()) -> state().
tick(State) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    State2 = maybe_reload_hashtrees(Ring, State),
    State3 = lists:foldl(fun(_,S) ->
                                 maybe_poke_tree(S)
                         end, State2, lists:seq(1,10)),
    State4 = maybe_exchange(Ring, State3),
    State4.

%% ~~~~~~~~~~~~~~~~~~~~ maybe_reload_hashtrees ~~~~~~~~~~~~~~~~~~~~
-spec maybe_reload_hashtrees(riak_core_ring(), state()) -> state().
maybe_reload_hashtrees(Ring, State) ->
    case lists:member(legolas, riak_core_node_watcher:services(node())) of
        true ->
            reload_hashtrees(Ring, State);
        false ->
            State
    end.

%% ~~~~~~~~~~~~~~~~~~~~ reload_hashtrees ~~~~~~~~~~~~~~~~~~~~
%% Determine the index_hashtree pid for each running primary vnode. This
%% function is called each tick to ensure that any newly spawned vnodes are
%% queried.
-spec reload_hashtrees(riak_core_ring(), state()) -> state().
reload_hashtrees(Ring, State=#state{trees=Trees}) ->
    Indices = riak_core_ring:my_indices(Ring),
    Existing = dict:from_list(Trees),
    MissingIdx = [Idx || Idx <- Indices,
                         not dict:is_key(Idx, Existing)],
    [legolas_vnode:request_hashtree_pid(Idx) || Idx <- MissingIdx],
    State.

%% ~~~~~~~~~~~~~~~~~~~~ maybe_poke_tree ~~~~~~~~~~~~~~~~~~~~
-spec maybe_poke_tree(state()) -> state().
maybe_poke_tree(State=#state{trees=[]}) ->
    State;
maybe_poke_tree(State) ->
    {Tree, State2} = next_tree(State),
    legolas_index_hashtree:poke(Tree),
    State2.

%% ~~~~~~~~~~~~~~~~~~~~ next_tree ~~~~~~~~~~~~~~~~~~~~
-spec next_tree(state()) -> {pid(), state()}.
next_tree(#state{trees=[]}) ->
    throw(no_trees_registered);
next_tree(State=#state{tree_queue=[], trees=Trees}) ->
    State2 = State#state{tree_queue=Trees},
    next_tree(State2);
next_tree(State=#state{tree_queue=Queue}) ->
    [{_Index,Pid}|Rest] = Queue,
    State2 = State#state{tree_queue=Rest},
    {Pid, State2}.

%%%===================================================================
%%% Exchanging
%%%===================================================================

%% ------------------------------ do_exchange_status ------------------------------ 
-spec do_exchange_status(pid(), vnode(), vnode(), index_n(), any(), state()) -> state().
do_exchange_status(_Pid, LocalVN, RemoteVN, IndexN, Reply, State) ->
    {LocalIdx, _} = LocalVN,
    {RemoteIdx, RemoteNode} = RemoteVN,
    case Reply of
        ok ->
            State;
        {remote, anti_entropy_disabled} ->
            lager:warning("Active anti-entropy is disabled on ~p", [RemoteNode]),
            State;
        _ ->
            State2 = requeue_exchange(LocalIdx, RemoteIdx, IndexN, State),
            State2
    end.

-spec enqueue_exchanges([exchange()], state()) -> state().
enqueue_exchanges(Exchanges, State) ->
    EQ = prune_exchanges(State#state.exchange_queue ++ Exchanges),
    State#state{exchange_queue=EQ}.

%% ~~~~~~~~~~~~~~~~~~~~ enqueue_exchange ~~~~~~~~~~~~~~~~~~~~
-spec enqueue_exchange(index() |
                       {index(), index_n()} |
                       {index(), index(), index_n()}, state()) -> state().
enqueue_exchange(E={Index, _RemoteIdx, _IndexN}, State) ->
    %% Verify that the exchange is valid
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Exchanges = all_pairwise_exchanges(Index, Ring),
    case lists:member(E, Exchanges) of
        true ->
            enqueue_exchanges([E], State);
        false ->
            State
    end;
enqueue_exchange({Index, IndexN}, State) -> 
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Exchanges = all_pairwise_exchanges(Index, Ring),
    Exchanges2 = [Exchange || Exchange={_, _, IdxN} <- Exchanges,
                              IdxN =:= IndexN],
    enqueue_exchanges(Exchanges2, State);
enqueue_exchange(Index, State) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Exchanges = all_pairwise_exchanges(Index, Ring),
    enqueue_exchanges(Exchanges, State).

%% ~~~~~~~~~~~~~~~~~~~~ start_exchange ~~~~~~~~~~~~~~~~~~~~
-spec start_exchange(vnode(),
                     {index(), index_n()},
                     riak_core_ring(),
                     state()) -> {any(), state()}.
start_exchange(LocalVN, {RemoteIdx, IndexN}, Ring, State) ->
    %% in rare cases, when the ring is resized, there may be an
    %% exchange enqueued for an index that no longer exists. catch
    %% the case here and move on
    try riak_core_ring:index_owner(Ring, RemoteIdx) of
        Owner ->
            Nodes = lists:usort([node(), Owner]),
            DownNodes = Nodes -- riak_core_node_watcher:nodes(legolas),
            case DownNodes of
                [] ->
                    RemoteVN = {RemoteIdx, Owner},
                    start_exchange(LocalVN, RemoteVN, IndexN, Ring, State);
                _ ->
                    {{legolas_down, DownNodes}, State}
            end
    catch
        error:{badmatch,_} ->
            lager:warning("ignoring exchange to non-existent index: ~p", [RemoteIdx]),
            {ok, State}
    end.

start_exchange(LocalVN, RemoteVN, IndexN, Ring, State) ->
    {LocalIdx, _} = LocalVN,
    {RemoteIdx, _} = RemoteVN,
    case riak_core_ring:vnode_type(Ring, LocalIdx) of
        primary ->
            case orddict:find(LocalIdx, State#state.trees) of
                error ->
                    %% The local vnode has not yet registered it's
                    %% index_hashtree. Likewise, the vnode may not even
                    %% be running (eg. after a crash).  Send request to
                    %% the vnode to trigger on-demand start and requeue
                    %% exchange.
                    legolas_vnode:request_hashtree_pid(LocalIdx),
                    State2 = requeue_exchange(LocalIdx, RemoteIdx, IndexN, State),
                    {not_built, State2};
                {ok, Tree} ->
                    case legolas_exchange_fsm:start(LocalVN, RemoteVN,
                                                    IndexN, Tree, self()) of
                        {ok, FsmPid} ->
                            Ref = monitor(process, FsmPid),
                            Exchanges = State#state.exchanges,
                            Exchanges2 = [{LocalIdx, Ref, FsmPid} | Exchanges],
                            {ok, State#state{exchanges=Exchanges2}};
                        {error, Reason} ->
                            {Reason, State}
                    end
            end;
        _ ->
            %% No longer owner of this partition or partition is
            %% part or larger future ring, ignore exchange
            {not_responsible, State}
    end.

%% ~~~~~~~~~~~~~~~~~~~~ all_pairwise_exchanges ~~~~~~~~~~~~~~~~~~~~
-spec all_pairwise_exchanges(index(), riak_core_ring())
                            -> [exchange()].
all_pairwise_exchanges(Index, Ring) ->
    LocalIndexN = legolas:responsible_preflists(Index, Ring),
    Sibs = legolas:preflist_siblings(Index),
    lists:flatmap(
      fun(RemoteIdx) ->
              RemoteIndexN = legolas:responsible_preflists(RemoteIdx, Ring),
              SharedIndexN = ordsets:intersection(ordsets:from_list(LocalIndexN),
                                                  ordsets:from_list(RemoteIndexN)),
              [{Index, RemoteIdx, IndexN} || IndexN <- SharedIndexN]
      end, Sibs).

%% ~~~~~~~~~~~~~~~~~~~~ all_exchanges ~~~~~~~~~~~~~~~~~~~~
-spec all_exchanges(node(), riak_core_ring(), state())
                   -> [exchange()].
all_exchanges(_Node, Ring, #state{trees=Trees}) ->
    Indices = orddict:fetch_keys(Trees),
    lists:flatmap(fun(Index) ->
                          all_pairwise_exchanges(Index, Ring)
                  end, Indices).

%% ~~~~~~~~~~~~~~~~~~~~ add_index_exchanges ~~~~~~~~~~~~~~~~~~~~
-spec add_index_exchanges(index(), state()) -> state().
add_index_exchanges(_Index, State) when State#state.mode == manual ->
    State;
add_index_exchanges(Index, State) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Exchanges = all_pairwise_exchanges(Index, Ring),
    EQ = State#state.exchange_queue ++ Exchanges,
    EQ2 = prune_exchanges(EQ),
    State#state{exchange_queue=EQ2}.

%% ~~~~~~~~~~~~~~~~~~~~ prune_exchanges ~~~~~~~~~~~~~~~~~~~~
-spec prune_exchanges([exchange()])
                     -> [exchange()].
prune_exchanges(Exchanges) ->
    L = [if A < B ->
                 {A, B, IndexN};
            true ->
                 {B, A, IndexN}
         end || {A, B, IndexN} <- Exchanges],
    lists:usort(L).

%% ~~~~~~~~~~~~~~~~~~~~ already_exchanging ~~~~~~~~~~~~~~~~~~~~
-spec already_exchanging(index() ,state()) -> boolean().
already_exchanging(Index, #state{exchanges=E}) ->
    case lists:keyfind(Index, 1, E) of
        false ->
            false;
        {Index,_,_} ->
            true
    end.

%% ~~~~~~~~~~~~~~~~~~~~ maybe_exchange ~~~~~~~~~~~~~~~~~~~~
-spec maybe_exchange(riak_core_ring(), state()) -> state().
maybe_exchange(Ring, State) ->
    case next_exchange(Ring, State) of
        {none, State2} ->
            State2;
        {NextExchange, State2} ->
            {LocalIdx, RemoteIdx, IndexN} = NextExchange,
            case already_exchanging(LocalIdx, State) of
                true ->
                    requeue_exchange(LocalIdx, RemoteIdx, IndexN, State2);
                false ->
                    LocalVN = {LocalIdx, node()},
                    case start_exchange(LocalVN, {RemoteIdx, IndexN}, Ring, State2) of
                        {ok, State3} ->
                            State3;
                        {_Reason, State3} ->
                            State3
                    end
            end
    end.

%% ~~~~~~~~~~~~~~~~~~~~ next_exchange ~~~~~~~~~~~~~~~~~~~~
-spec next_exchange(riak_core_ring(), state()) -> {'none' | exchange(), state()}.
next_exchange(_Ring, State=#state{exchange_queue=[], trees=[]}) ->
    {none, State};
next_exchange(_Ring, State=#state{exchange_queue=[],
                                  mode=Mode}) when Mode == manual ->
    {none, State};
next_exchange(Ring, State=#state{exchange_queue=[]}) ->
    case prune_exchanges(all_exchanges(node(), Ring, State)) of
        [] ->
            {none, State};
        [Exchange|Rest] ->
            State2 = State#state{exchange_queue=Rest},
            {Exchange, State2}
    end;
next_exchange(_Ring, State=#state{exchange_queue=Exchanges}) ->
    [Exchange|Rest] = Exchanges,
    State2 = State#state{exchange_queue=Rest},
    {Exchange, State2}.

%% ~~~~~~~~~~~~~~~~~~~~ requeue_poke ~~~~~~~~~~~~~~~~~~~~
-spec requeue_poke(index(), state()) -> state().
requeue_poke(Index, State=#state{trees=Trees}) ->
    case orddict:find(Index, Trees) of
        {ok, Tree} ->
            Queue = State#state.tree_queue ++ [{Index,Tree}],
            State#state{tree_queue=Queue};
        _ ->
            State
    end.

%% ~~~~~~~~~~~~~~~~~~~~ requeue_exchange ~~~~~~~~~~~~~~~~~~~~
-spec requeue_exchange(index(), index(), index_n(), state()) -> state().
requeue_exchange(LocalIdx, RemoteIdx, IndexN, State) ->
    Exchange = {LocalIdx, RemoteIdx, IndexN},
    case lists:member(Exchange, State#state.exchange_queue) of
        true ->
            State;
        false ->
            lager:debug("Requeue: ~p", [{LocalIdx, RemoteIdx, IndexN}]),
            Exchanges = State#state.exchange_queue ++ [Exchange],
            State#state{exchange_queue=Exchanges}
    end.

