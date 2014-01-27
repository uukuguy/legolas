%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas存储虚拟节点。
%%%
%%% @end
%%% Created : 2013-11-04 22:18:49
%%%------------------------------------------------------------ 

-module(legolas_storage_vnode).
-behaviour(riak_core_vnode).
-include("legolas.hrl").
-include("legolas_storage_vnode.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
         start_vnode/1,
         put_data/4,
         get_data/3,
         delete_data/3,
         fold_data/3
        ]).

%% ------------------------------ Callbacks ------------------------------ 
-export([
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3,
         handle_info/2
        ]).

%% ------------------------------ record ------------------------------ 
-record(state, 
        {
         partition,
         storage_backend :: module(),
         backend_state :: term(),
         async_folding :: boolean(),
         vnodeid :: undefined | binary(),
         in_handoff = false :: boolean(),
         handoff_target :: node(),
         forward :: node() | [{integer(), node()}],
         hashtrees :: pid()
        }).

%-record(handoff_target,
        %{
         %type :: atom(),
         %keyspace :: {integer(), node()}
        %}).

-type state() :: #state{}.

-define(MASTER, legolas_storage_vnode_master).
-define(DEFAULT_HASHTREE_TOKENS, 90).

%% ============================== APIs ==============================
%%

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

%% ------------------------------ put_data ------------------------------ 
%% @doc 异步调用完成写入数据操作。实际操作在handle_command({put_data, ...}, ...)中处理。

put_data(Preflist, ReqId, Path, Data) ->
    riak_core_vnode_master:command(Preflist,
                                   {put_data, ReqId, Path, Data},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% ------------------------------ put_data ------------------------------ 
%% @doc 异步调用完成读出数据操作。实际操作在handle_command({get_data, ...}, ...)中处理。

get_data(Preflist, ReqId, Path) ->
    riak_core_vnode_master:command(Preflist,
                                   {get_data, ReqId, Path},
                                   {fsm, undefined, self()},
                                   ?MASTER).
%% ------------------------------ delete_data ------------------------------ 
%% @doc 异步调用完成删除数据操作。实际操作在handle_command({delete_data, ...}, ...)中处理。

delete_data(Preflist, ReqId, Path) ->
    riak_core_vnode_master:command(Preflist,
                                   {delete_data, ReqId, Path},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% ------------------------------ fold_data ------------------------------ 
%% @doc 同步调用完成遍历数据操作。实际操作在handle_command(?FOLD_REQ(...))中处理。

fold_data(Preflist, Fun, Acc0) ->
    Req = riak_core_util:make_fold_req(Fun, Acc0),
    riak_core_vnode_master:sync_spawn_command(Preflist,
                                              Req,
                                              ?MASTER).

%% ============================== Callbacks ==============================
%%

%% ------------------------------ init ------------------------------ 
%% @doc 从参数文件中获得存储后端StorageBackend，并启动存储后端。
%%      初始化异步遍历AsyncFolding支持。
%%
init([Partition]) ->
    ?DEBUG("Starting VNode. Partition: ~p", [integer_to_list(Partition, 16)]),

    StorageBackend = app_helper:get_env(legolas, storage_backend),
    Configuration = app_helper:get_env(legolas),
    AsyncFolding = app_helper:get_env(legolas, async_folds, true) == true,
    WorkerPoolSize = app_helper:get_env(legolas, worker_pool_size, 10),

    Result = case catch StorageBackend:start(Partition, Configuration) of
                 {ok, BackendState} ->
                     State = #state{partition=Partition,
                                    storage_backend=StorageBackend,
                                    backend_state=BackendState,
                                    async_folding=AsyncFolding},
                     %% 异步遍历
                     case AsyncFolding of
                         true ->
                             %% Create worker pool initialization tuple
                             FoldWorkerPool = {pool, legolas_worker, WorkerPoolSize, []},
                             State2 = maybe_create_hashtrees(State),
                             {ok, State2, [FoldWorkerPool]};
                             %{ok, State};
                         false ->
                             {ok, State}
                     end;
                 {error, Reason} ->
                     ?ERROR("Failed to start ~p backend for partition ~p error: ~p",
                            [StorageBackend, integer_to_list(Partition, 16), Reason]),
                     riak:stop("backend module failed to start."),
                     {error, Reason};
                 {'EXIT', Reason1} ->
                     ?ERROR("Failed to start ~p backend for partition ~p crash: ~p",
                            [StorageBackend, integer_to_list(Partition, 16), Reason1]),
                     riak:stop("backend module failed to start."),
                     {error, Reason1}
             end,

    ?DEBUG("VNode started. Partition: ~p", [integer_to_list(Partition, 16)]),
    Result.

%% ------------------------------ handle_command put_data ------------------------------ 
%% @doc StorageBackend中的调用参数要求Bucket，Key方式，此时取Bucket = <<>>。
%%
handle_command({put_data, ReqId, Path, Data}, _Sender, 
               #state{
                  storage_backend=StorageBackend,
                  backend_state=BackendState
                 } = State) ->
    ?DEBUG("command: put_data, ReqId: ~p Path: ~p", [ReqId, Path]),
    Bucket = <<>>,
    IndexSpecs = [],
    Result = case StorageBackend:put(Bucket, Path, IndexSpecs, Data, BackendState) of
                 {ok, _PutState} -> ok;
                 {error, Reason, _ErrorState} -> 
                     ?ERROR("command: get_data, ReqId: ~p Path: ~p Reason: ~p", [ReqId, Path, Reason]),
                     {error, Reason}
             end,
    {reply, {ok, ReqId, Result}, State};

%% ------------------------------ handle_command get_data ------------------------------ 
%% @doc StorageBackend中的调用参数要求Bucket，Key方式，此时取Bucket = <<>>。
%%
handle_command({get_data, ReqId, Path}, _Sender, 
               #state{
                  storage_backend=StorageBackend, 
                  backend_state=BackendState
                 } = State) ->
    Bucket = <<>>,
    Result = case StorageBackend:get(Bucket, Path, BackendState) of
                 {ok, Data, _GetState} -> 
                     ?DEBUG("command: get_data, ReqId: ~p Data: ~p", [ReqId, Data]),
                     {ok, Data};
                 {error, Reason, _} -> 
                     ?ERROR("command: get_data, ReqId: ~p Path: ~p Reason: ~p", [ReqId, Path, Reason]),
                     {error, Reason}
             end,
    {reply, {ok, ReqId, Result}, State};

%% ------------------------------ handle_command delete_data ------------------------------ 
%% @doc StorageBackend中的调用参数要求Bucket，Key方式，此时取Bucket = <<>>。
%%
handle_command({delete_data, ReqId, Path}, _Sender, 
               #state{
                  storage_backend=StorageBackend, 
                  backend_state=BackendState
                 } = State) ->
    ?DEBUG("command: delete_data, ReqId: ~p Path: ~p", [ReqId, Path]),
    Bucket = <<>>,
    IndexSpecs = [],
    Result = case StorageBackend:delete(Bucket, Path, IndexSpecs, BackendState) of
                 {ok, _DeleteState} ->
                     ?DEBUG("command: delete_data, ReqId: ~p", [ReqId]),
                     ok;
                 {error, Reason, _} -> 
                     ?ERROR("command: delete_data, ReqId: ~p", [ReqId]),
                     {error, Reason}
             end,
    {reply, {ok, ReqId, Result}, State};

%% ------------------------------ handle_command hashtree_pid ------------------------------ 
%% @doc entropy exchange commands
%%
handle_command({hashtree_pid, Node}, _, State=#state{hashtrees=HT}) ->
    %% Handle riak_core request forwarding during ownership handoff.
    case node() of
        Node ->
            %% Following is necessary in cases where anti-entropy was enabled
            %% after the vnode was already running
            case HT of
                undefined ->
                    State2 = maybe_create_hashtrees(State),
                    {reply, {ok, State2#state.hashtrees}, State2};
                _ ->
                    {reply, {ok, HT}, State}
            end;
        _ ->
            {reply, {error, wrong_node}, State}
    end;

%% ------------------------------ handle_command rehash ------------------------------ 
%% @doc
%%
handle_command({rehash, Bucket, Key}, _, #state{storage_backend=StorageBackend, backend_state=BackendState} = State) ->
    case StorageBackend:get(Bucket, Key, BackendState) of
        {ok, Bin, _UpdModState} ->
            update_hashtree(Bucket, Key, Bin, State);
        _ ->
            %% Make sure hashtree isn't tracking deleted data
            legolas_index_hashtree:delete({Bucket, Key}, State#state.hashtrees)
    end,
    {noreply, State};


%% ------------------------------ handle_command ?FOLD_REQ ------------------------------ 
%% @doc -define(FOLD_REQ, #riak_core_fold_req_v1). Defined in riak_core_vnode.html.
%%
handle_command(?FOLD_REQ{foldfun=FoldFun, 
                         acc0=Acc0
                         %forwardable=_Forwardable, 
                         %opts=Opts
                        }, Sender, State) ->
    %% The riak_core layer takes care of forwarding/not forwarding, so
    %% we ignore forwardable here.
    %%
    %% The function in riak_core used for object folding expects the
    %% bucket and key pair to be passed as the first parameter, but in
    %% legolas the bucket and key have been separated. This function
    %% wrapper is to address this mismatch.
    ?DEBUG("handle FOLD_REQ foldfun: ~p acc0: ~p", [FoldFun, Acc0]),
    FoldWrapper = fun(Bucket, Key, Value, Acc) ->
                          FoldFun({Bucket, Key}, Value, Acc)
                  end,
    Opts = [],
    do_fold_data(FoldWrapper, Acc0, Sender, Opts, State).

%% @private
do_fold_data(Fun, Acc0, Sender, ReqOpts, #state{
                                            async_folding=AsyncFolding,
                                            storage_backend=StorageBackend,
                                            backend_state=BackendState
                                           } = State) ->
    {ok, Capabilities} = StorageBackend:capabilities(BackendState),
    AsyncBackend = lists:member(async_fold, Capabilities),
    case AsyncFolding andalso AsyncBackend of
        true ->
            Opts = [async_fold|ReqOpts];
        false ->
            Opts = ReqOpts
    end,
    case StorageBackend:fold_objects(Fun, Acc0, Opts, BackendState) of
        {ok, Acc} ->
            {reply, Acc, State};
        {async, Folder} ->
            FinishFun =
                fun(Acc) ->
                        riak_core_vnode:reply(Sender, Acc)
                end,
            {async, {fold, Folder, FinishFun}, Sender, State};
        ER ->
            {reply, ER, State}
    end.

%% ------------------------------ handle_handoff_command ------------------------------ 
%%
%%
%% While in handoff, vnodes have the option of returning {forward, State}
%% which will cause riak_core to forward the request to the handoff target
%% node. For legolas, we issue a put locally as well as forward it in case
%% the vnode has already handed off the previous version. All other requests
%% are handled locally and not forwarded since the relevant data may not have
%% yet been handed off to the target node. Since we do not forward deletes it
%% is possible that we do not clear a tombstone that was already handed off.
%% This is benign as the tombstone will eventually be re-deleted.
handle_handoff_command(Req=put_data, Sender, State) ->
    ?DEBUG("handle_handoff_command forward. Req = ~p Sender = ~p", [Req, Sender]),
    {noreply, NewState} = handle_command(Req, Sender, State),
    {forward, NewState};
%% Handle all unspecified cases locally without forwarding
handle_handoff_command(Req, Sender, State) ->
    handle_command(Req, Sender, State).

%% ------------------------------ handoff_starting ------------------------------ 
%% @doc
%%
handoff_starting({HOType, {Partition, Node}} = HandoffTarget, State) ->
    ?DEBUG("HoType: ~p Partition: ~p Node: ~p", [HOType, integer_to_list(Partition, 16), Node]),
    {true, State#state{in_handoff=true, handoff_target=HandoffTarget }}.

%% ------------------------------ handoff_cancelled ------------------------------ 
%% @doc
%%
handoff_cancelled(#state{handoff_target=HandoffTarget} = State) ->
    case HandoffTarget of
        {HOType, {Partition, Node}} ->
            ?DEBUG("HoType: ~p Partition: ~p Node: ~p", [HOType, integer_to_list(Partition, 16), Node]);
        undefined ->
            ?WARNING("handoff_cancelled handoff_target = undeined", [])
    end,
    {ok, State#state{in_handoff=false, handoff_target=undefined}}.

%% ------------------------------ handoff_finished ------------------------------ 
%% @doc
%%
handoff_finished({Partition, Node} = _HandoffTarget, State) ->
    ?DEBUG("Partition: ~p Node: ~p", [integer_to_list(Partition, 16), Node]),
    {ok, State#state{in_handoff=false, handoff_target=undefined}}.

%% ------------------------------ hand_handoff_data ------------------------------ 
%% @doc 节点环变化引起的数据迁移,接收端解码并保存数据在本地。
%%
handle_handoff_data(Binary, #state{storage_backend=StorageBackend, backend_state=BackendState} = State) ->
    PartitionString = StorageBackend:get_partition_string(BackendState),
    ?DEBUG("Enter handle_handoff_data/2. Partition: ~p Data: ~p", [PartitionString, Binary]),
    try
        {BKey, Data} = decode_binary_object(Binary),
        {Bucket, Path} = BKey,
        IndexSpecs = [],
        Result = case StorageBackend:put(Bucket, Path, IndexSpecs, Data, BackendState) of
                     {ok, _PutState} -> ok;
                     {error, Reason, _ErrorState} -> 
                         ?ERROR("Bucket: ~p Path: ~p Reason: ~p", [Bucket, Path, Reason]),
                         {error, Reason}
                 end,
        {reply, Result, State}
    catch Error:Reason2 ->
            ?WARNING("Unreadable object discarded in handoff: ~p:~p",
                          [Error, Reason2]),
            {reply, {error, [Error, Reason2]}, State}
    end.

%% @private
%% Decode a binary object. We first try to interpret the data as a "new format" object which indicates
%% its encoding method, but if that fails we use the legacy zlib and protocol buffer decoding:
decode_binary_object(BinaryObject) ->
    try binary_to_term(BinaryObject) of
        { Method, BinObj } ->
            ?DEBUG("decode_binary_object Method: ~p BinObj: ~p", [Method, BinObj]),
            case Method of
                encode_raw  -> {{B, K}, Val} = BinObj,
                               BKey = {B, K},
                               {BKey, Val};

                _           -> lager:error("Invalid handoff encoding ~p", [Method]),
                               throw(invalid_handoff_encoding)
            end;

        _                   ->  lager:error("Request to decode invalid handoff object"),
                                throw(invalid_handoff_object)

    %% An exception means we have a legacy handoff object:
    catch
        _:_                 -> do_zlib_decode(BinaryObject)
    end.

do_zlib_decode(BinaryObject) ->
    DecodedObject = zlib:unzip(BinaryObject),
    _PBObj = riak_core_pb:decode_riakobject_pb(DecodedObject).
    %BKey = {PBObj#riakobject_pb.bucket,PBObj#riakobject_pb.key},
    %{BKey, PBObj#riakobject_pb.val}.

%% ------------------------------ encode_handoff_item ------------------------------ 
%% @doc 节点环变化引起的数据迁移,发送端编码准备发送。
%%
encode_handoff_item(Path, Data) ->
    %% before sending data to another node change binary version
    %% to one supported by the cluster. This way we don't send
    %% unsupported formats to old nodes
    ?DEBUG("Enter encode_handoff_item/2. Path: ~p Data: ~p", [Path, Data]),
    try
        encode_binary_object(Path, Data)
    catch Error:Reason ->
            lager:warning("Handoff encode failed: ~p:~p",
                          [Error,Reason]),
            corrupted
    end.

%% @private
encode_binary_object(Path, Data) ->
    EncodedObject = { Path, iolist_to_binary(Data) },
    Method = encode_raw,
    return_encoded_binary_object(Method, EncodedObject).

    %Method = handoff_data_encoding_method(),

    %case Method of
        %encode_raw  -> 
            %EncodedObject = { Path, iolist_to_binary(Data) },
            %return_encoded_binary_object(Method, EncodedObject)

        %% zlib encoding is a special case, we return the legacy format:
        %encode_zlib -> 
            %PBEncodedObject = riak_core_pb:encode_riakobject_pb(#riakobject_pb{bucket=Bucket, key=Key, val=Value}),
            %zlib:zip(PBEncodedObject)
    %end.

%handoff_data_encoding_method() ->
    %riak_core_capability:get({legolas, handoff_data_encoding}, encode_zlib).

%% Return objects in a consistent form:
return_encoded_binary_object(Method, EncodedObject) ->
    term_to_binary({ Method, EncodedObject }).

%% ------------------------------ is_empty ------------------------------ 
%% @doc
%%
is_empty(#state{storage_backend=StorageBackend, backend_state=BackendState}=State) ->
    %?DEBUG("is_empty() State = ~p", [State]),
    IsEmpty = StorageBackend:is_empty(BackendState),
    case IsEmpty of
        true ->
            {true, State};
        false ->
            Size = maybe_calc_handoff_size(State),
            {false, Size, State}
    end.

%% ------------------------------ maybe_calc_handoff_size ------------------------------ 
%% @doc
%%
maybe_calc_handoff_size(#state{storage_backend=StorageBackend,backend_state=BackendState}) ->
    {ok, Capabilities} = StorageBackend:capabilities(BackendState),
    case lists:member(size, Capabilities) of
        true -> StorageBackend:data_size(BackendState);
        false -> undefined
    end.

delete(State) ->
    {ok, State}.

%% ------------------------------ handle_coverage ------------------------------ 
%% @doc
%%
handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

%% ------------------------------ handle_exit ------------------------------ 
%% @doc
%%
handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

%% ------------------------------ terminate ------------------------------ 
%% @doc
%%
terminate(_Reason, _State) ->
    ok.

%% ------------------------------ handle_info ------------------------------ 
%% @doc
%%
handle_info(retry_create_hashtree, State=#state{hashtrees=undefined}) ->
    State2 = maybe_create_hashtrees(State),
    case State2#state.hashtrees of
        undefined ->
            ok;
        _ ->
            ?INFO("legolas/~p: successfully started index_hashtree on retry",
                  [State#state.partition])
    end,
    {ok, State2};
handle_info(retry_create_hashtree, State) ->
    {ok, State};
handle_info({'DOWN', _, _, Pid, _}, State=#state{hashtrees=Pid}) ->
    State2 = State#state{hashtrees=undefined},
    State3 = maybe_create_hashtrees(State2),
    {ok, State3};
handle_info({'DOWN', _, _, _, _}, State) ->
    {ok, State}.

%% ============================== Internal functions ==============================
%%

%% @private
-spec update_hashtree(binary(), binary(), binary(), state()) -> ok.
update_hashtree(Bucket, Key, Val, #state{hashtrees=Trees}) ->
    case get_hashtree_token() of
        true ->
            legolas_index_hashtree:async_insert_object({Bucket, Key}, Val, Trees),
            ok;
        false ->
            legolas_index_hashtree:insert_object({Bucket, Key}, Val, Trees),
            put(hashtree_tokens, max_hashtree_tokens()),
            ok
    end.

%% @private
get_hashtree_token() ->
    Tokens = get(hashtree_tokens),
    case Tokens of
        undefined ->
            put(hashtree_tokens, max_hashtree_tokens() - 1),
            true;
        N when N > 0 ->
            put(hashtree_tokens, Tokens - 1),
            true;
        _ ->
            false
    end.

%% @private
-spec max_hashtree_tokens() -> pos_integer().
max_hashtree_tokens() ->
    app_helper:get_env(legolas,
                       anti_entropy_max_async, 
                       ?DEFAULT_HASHTREE_TOKENS).

%% @private
-spec maybe_create_hashtrees(state()) -> state().
maybe_create_hashtrees(State) ->
    maybe_create_hashtrees(legolas_entropy_manager:enabled(), State).

-spec maybe_create_hashtrees(boolean(), state()) -> state().
maybe_create_hashtrees(false, State) ->
    State;
maybe_create_hashtrees(true, State=#state{partition=Partition}) ->
    %% Only maintain a hashtree if a primary vnode
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    case riak_core_ring:vnode_type(Ring, Partition) of
        primary ->
            RP = legolas:responsible_preflists(Partition),
            case legolas_index_hashtree:start(Partition, RP, self()) of
                {ok, Trees} ->
                    monitor(process, Trees),
                    State#state{hashtrees=Trees};
                Error ->
                    ?INFO("legolas/~p: unable to start index_hashtree: ~p",
                               [Partition, Error]),
                    erlang:send_after(1000, self(), retry_create_hashtree),
                    State#state{hashtrees=undefined}
            end;
        _ ->
            State
    end.

