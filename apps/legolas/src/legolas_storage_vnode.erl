%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-04 22:18:49
%%%------------------------------------------------------------ 

-module(legolas_storage_vnode).
-behaviour(riak_core_vnode).
-include("legolas.hrl").

-export([start_vnode/1,
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
         handle_exit/3]).

-export([
         put_data/4,
         get_data/3,
         delete_data/3
        ]).

-record(state, 
        {
         partition,
         storage_backend :: module(),
         backend_state :: term(),
         async_folding :: boolean(),
         vnodeid :: undefined | binary(),
         in_handoff = false :: boolean(),
         handoff_target :: node(),
         forward :: node() | [{integer(), node()}]
        }).

-define(MASTER, legolas_storage_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

put_data(Preflist, ReqId, Path, Data) ->
    riak_core_vnode_master:command(Preflist,
                                   {put_data, ReqId, Path, Data},
                                   {fsm, undefined, self()},
                                   ?MASTER).

get_data(Preflist, ReqId, Path) ->
    riak_core_vnode_master:command(Preflist,
                                   {get_data, ReqId, Path},
                                   {fsm, undefined, self()},
                                   ?MASTER).
delete_data(Preflist, ReqId, Path) ->
    riak_core_vnode_master:command(Preflist,
                                   {delete_data, ReqId, Path},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%%%------------------------------------------------------------ 
%%% Callbacks
%%%------------------------------------------------------------ 

init([Partition]) ->
    %?NOTICE("Starting VNode. Partition: ~p", [Partition]),
    StorageBackend = app_helper:get_env(legolas, storage_backend),
    Configuration = app_helper:get_env(legolas),
    AsyncFolding = app_helper:get_env(legolas, async_folds, true) == true,
    %app_helper:get_prop_or_env(data_root, Config, eleveldb),
    Result = case catch StorageBackend:start(Partition, Configuration) of
                 {ok, BackendState} ->
                     %% Get the backend capabilities
                     State = #state{partition=Partition,
                                    storage_backend=StorageBackend,
                                    backend_state=BackendState,
                                    async_folding=AsyncFolding},
                     {ok, State};
                 {error, Reason} ->
                     ?ERROR("Failed to start ~p backend for partition ~p error: ~p",
                            [StorageBackend, Partition, Reason]),
                     riak:stop("backend module failed to start."),
                     {error, Reason};
                 {'EXIT', Reason1} ->
                     ?ERROR("Failed to start ~p backend for partition ~p crash: ~p",
                            [StorageBackend, Partition, Reason1]),
                     riak:stop("backend module failed to start."),
                     {error, Reason1}
             end,
    %?NOTICE("VNode started. Partition: ~p", [Partition]),
    Result.

handle_command({put_data, ReqId, Path, Data}, _Sender, 
               #state{
                  storage_backend=StorageBackend,
                  backend_state=BackendState
                 } = State) ->
    ?NOTICE("command: put_data, ReqId: ~p Path: ~p", [ReqId, Path]),
    Bucket = <<>>,
    IndexSpecs = [],
    Result = case StorageBackend:put(Bucket, Path, IndexSpecs, Data, BackendState) of
                 {ok, _PutState} -> ok;
                 {error, Reason} -> 
                     ?ERROR("command: get_data, ReqId: ~p Path: ~p Reason: ~p", [ReqId, Path, Reason]),
                     {error, Reason}
             end,
    {reply, {ok, ReqId, Result}, State};

handle_command({get_data, ReqId, Path}, _Sender, 
               #state{
                  storage_backend=StorageBackend, 
                  backend_state=BackendState
                 } = State) ->
    Bucket = <<>>,
    Result = case StorageBackend:get(Bucket, Path, BackendState) of
                 {ok, Data, _GetState} -> 
                     ?NOTICE("command: get_data, ReqId: ~p Data: ~p", [ReqId, Data]),
                     {ok, Data};
                 {error, Reason} -> 
                     ?ERROR("command: get_data, ReqId: ~p Path: ~p Reason: ~p", [ReqId, Path, Reason]),
                     {error, Reason}
             end,
    {reply, {ok, ReqId, Result}, State};

handle_command({delete_data, ReqId, Path}, _Sender, 
               #state{
                  storage_backend=StorageBackend, 
                  backend_state=BackendState
                 } = State) ->
    ?NOTICE("command: delete_data, ReqId: ~p Path: ~p", [ReqId, Path]),
    Bucket = <<>>,
    IndexSpecs = [],
    Result = case StorageBackend:delete(Bucket, Path, IndexSpecs, BackendState) of
                 {ok, _DeleteState} ->
                     ?NOTICE("command: delete_data, ReqId: ~p", [ReqId]),
                     ok;
                 _ -> 
                     ?ERROR("command: delete_data, ReqId: ~p", [ReqId]),
                     {error, ""}
             end,
    {reply, {ok, ReqId, Result}, State}.

%% While in handoff, vnodes have the option of returning {forward, State}
%% which will cause riak_core to forward the request to the handoff target
%% node. For riak_kv, we issue a put locally as well as forward it in case
%% the vnode has already handed off the previous version. All other requests
%% are handled locally and not forwarded since the relevant data may not have
%% yet been handed off to the target node. Since we do not forward deletes it
%% is possible that we do not clear a tombstone that was already handed off.
%% This is benign as the tombstone will eventually be re-deleted.
handle_handoff_command(Req=put_data, Sender, State) ->
    ?NOTICE("handle_handoff_command forward. Req = ~p Sender = ~p", [Req, Sender]),
    {noreply, NewState} = handle_command(Req, Sender, State),
    {forward, NewState};
%% Handle all unspecified cases locally without forwarding
handle_handoff_command(Req, Sender, State) ->
    %?NOTICE("handle_handoff_command no forward. Req = ~p Sender = ~p State = ~p", [Req, Sender, State]),
    handle_command(Req, Sender, State).

handoff_starting(TargetNode, State) ->
    ?NOTICE("handoff_starting TargetNode = ~p", [TargetNode]),
    {true, State#state{in_handoff=true, handoff_target=TargetNode}}.

handoff_cancelled(State) ->
    {ok, State#state{in_handoff=false, handoff_target=undefined}}.

handoff_finished(TargetNode, State) ->
    ?NOTICE("handoff_finished TargetNode = ~p", [TargetNode]),
    {ok, State#state{in_handoff=false, handoff_target=undefined}}.

handle_handoff_data(Binary, #state{storage_backend=StorageBackend} = State) ->
    try
        {BKey, Val} = decode_binary_object(Binary),
        {B, K} = BKey,
        StorageBackend:put(K, Val),
        {reply, ok, State}
    catch Error:Reason2 ->
            lager:warning("Unreadable object discarded in handoff: ~p:~p",
                          [Error, Reason2]),
            {reply, ok, State}
    end.

encode_handoff_item(Path, Data) ->
    %% before sending data to another node change binary version
    %% to one supported by the cluster. This way we don't send
    %% unsupported formats to old nodes
    try
        encode_binary_object(Path, Data)
    catch Error:Reason ->
            lager:warning("Handoff encode failed: ~p:~p",
                          [Error,Reason]),
            corrupted
    end.

is_empty(State) ->
    %?NOTICE("is_empty() State = ~p", [State]),
    {false, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%%%------------------------------------------------------------ 
%%% Internal functions
%%%------------------------------------------------------------ 


%% @private
%% Encoding and decoding selection:

%% @private
%handoff_data_encoding_method() ->
    %riak_core_capability:get({riak_kv, handoff_data_encoding}, encode_zlib).

%% Decode a binary object. We first try to interpret the data as a "new format" object which indicates
%% its encoding method, but if that fails we use the legacy zlib and protocol buffer decoding:
decode_binary_object(BinaryObject) ->
    try binary_to_term(BinaryObject) of
        { Method, BinObj } ->
            case Method of
                encode_raw  -> {B, K, Val} = BinObj,
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

encode_binary_object(Path, Data) ->
    EncodedObject = { Path, iolist_to_binary(Data) },
    Method = encoe_raw,
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

%% Return objects in a consistent form:
return_encoded_binary_object(Method, EncodedObject) ->
    term_to_binary({ Method, EncodedObject }).
