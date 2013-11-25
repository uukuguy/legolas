%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-24 20:12:24
%%%------------------------------------------------------------ 

-module(legolas_fileblock_backend).
-include("legolas.hrl").
-behavior(legolas_backend).

%% Legolas Storage Backend API
-export([api_version/0,
         capabilities/1,
         capabilities/2,
         start/2,
         stop/1,
         get/3,
         put/5,
         delete/4,
         drop/1,
         fold_buckets/4,
         fold_keys/4,
         fold_objects/4,
         is_empty/1,
         status/1,
         callback/3]).

-define(API_VERSION, 1).
-define(CAPABILITIES, [async_fold]).

-record(state, {
          partition,
          partition_datadir, %% string()
          partition_string, %% string()
          default_size = 0,
          key_count = 0,
          bprefix_list = [],
          op_get = 0,
          op_put = 0,
          op_delete = 0
         }).
-type state() :: #state{}.
-type config() :: [{atom(), term()}].

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Return the major version of the
%% current API.
-spec api_version() -> {ok, integer()}.
api_version() ->
    {ok, ?API_VERSION}.

%% @doc Return the capabilities of the backend.
-spec capabilities(state()) -> {ok, [atom()]}.
capabilities(_) ->
    {ok, ?CAPABILITIES}.

%% @doc Return the capabilities of the backend.
-spec capabilities(riak_object:bucket(), state()) -> {ok, [atom()]}.
capabilities(_, _) ->
    {ok, ?CAPABILITIES}.

get_partition_string(Partition) ->
    PartitionString = common_utils:integer_to_fixed_list(Partition, 16, 160),
    PartitionString.

get_root_datadir() ->
    DataRootDir = code:priv_dir(legolas) ++ "/data",
    DataRootDir.

%get_partition_datadir(Partition) ->
    %RootDataDir = get_root_datadir(),
    %PartitionString = get_partition_string(Partition),
    %DataDir =  RootDataDir ++ "/" ++ PartitionString,
    %DataDir.

get_full_filename_in_partition(Path, #state{partition_datadir=DataDir}) ->
    Filename = DataDir ++ "/" ++ common_utils:binary_to_string(erlang:md5(Path)),
    Filename.

%% @doc Start this backend, yes, sir!
-spec start(integer(), config()) -> {ok, state()} | {error, term()}.
start(Partition, Config) ->
    %?NOTICE("Starting fileblock backend. Partition: ~p", [Partition]),
    DefaultLen = case app_helper:get_prop_or_env(
                        yessir_default_size, Config, yessir_backend) of
                     undefined -> 1024;
                     Len       -> Len
                 end,
    KeyCount = case common_utils:get_nested_config(
                      yessir_key_count, Config, yessir_backend) of
                   undefined -> 1024;
                   Count     -> Count
               end,
    BPrefixList = case app_helper:get_prop_or_env(
                        yessir_bucket_prefix_list, Config, yessir_backend) of
                     undefined -> [];
                     BPL       -> BPL
                 end,
    RootDataDir = get_root_datadir(),
    PartitionString = get_partition_string(Partition),
    DataDir = RootDataDir ++ "/" ++ PartitionString,
    case filelib:ensure_dir(DataDir ++ "/") of
        ok ->
            {ok, #state{partition = Partition,
                        partition_datadir = DataDir,
                        partition_string = PartitionString,
                        default_size = DefaultLen,
                        key_count = KeyCount,
                        bprefix_list = BPrefixList}};
        {error, Reason} ->
            ?ERROR("Error in ensure_datadir/1! Reason: ~p Path: ~p", [Reason, DataDir]),
            {error, Reason}
    end.
    %Result = case common_utils:ensure_path(DataDir) of
                 %{error, Reason} -> 
                     %?ERROR("Error in ensure_datadir/1! Reason: ~p Path: ~p", [Reason, DataDir]),
                     %{error, Reason};
                 %{ok, _} ->
                     %{ok, #state{partition = Partition,
                                 %partition_datadir = DataDir,
                                 %partition_string = PartitionString,
                                 %default_size = DefaultLen,
                                 %key_count = KeyCount,
                                 %bprefix_list = BPrefixList}}
             %end,
    %?NOTICE("fileblock backend started. Partition: ~p", [Partition]),
    %Result.

%% @doc Stop this backend, yes, sir!
-spec stop(state()) -> ok.
stop(_State) ->
    ok.

%% @doc Get a fake object, yes, sir!
-spec get(riak_object:bucket(), riak_object:key(), state()) ->
                 {ok, any(), state()} | {error, term()}.
get(Bucket, Key, S) ->
    get_object(Bucket, Key, true, S).

get_object(Bucket, Key, WantsBinary, S) ->
    get_object_bprefix(S#state.bprefix_list, Bucket, Key, WantsBinary, S).

get_object_bprefix([], _Bucket, Key, _WantsBinary, #state{op_get = Gets} = S) ->
    case do_get_data(Key, S) of
        {ok, Data} ->
            {ok, Data, S#state{op_get = Gets + 1} };
        {error, Reason} ->
            {error, Reason}
    end;

get_object_bprefix([{P, {_Mod, _Fun}}|Ps], Bucket, Key, WantsBinary, #state{} = S) ->
    P_len = byte_size(P),
    case Bucket of
        <<P:P_len/binary, _Rest/binary>> ->
            get_object_bprefix([], Bucket, Key, WantsBinary, S);
        _ ->
            get_object_bprefix(Ps, Bucket, Key, WantsBinary, S)
    end.


%% @doc Store an object.
-type index_spec() :: {add, Index, SecondaryKey} | {remove, Index, SecondaryKey}.
-spec put(riak_object:bucket(), riak_object:key(), [index_spec()], binary(), state()) ->
                 {ok, state()} | {error, term()}.
put(_Bucket, PKey, _IndexSpecs, Val, #state{op_put = Puts} = S) ->
    case do_put_data(PKey, Val, S) of
        ok -> {ok, S#state{op_put = Puts + 1}};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Delete an object, yes, sir!
-spec delete(riak_object:bucket(), riak_object:key(), [index_spec()], state()) ->
                    {ok, state()} | {error, term()}.
delete(_Bucket, Key, _IndexSpecs, #state{op_delete = Deletes} = S) ->
    case do_delete_data(Key, S) of
        ok -> {ok, S#state{op_delete = Deletes + 1}};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Fold over all the buckets, yes, sir!
-spec fold_buckets(legolas_backend:fold_buckets_fun(),
                   any(),
                   [],
                   state()) -> {ok, any()}.
fold_buckets(_FoldBucketsFun, Acc, _Opts, _S) ->
    {ok, Acc}.

%% @doc Fold over all the keys for one or all buckets, yes, sir!
-spec fold_keys(legolas_backend:fold_keys_fun(),
                any(),
                [{atom(), term()}],
                state()) -> {ok, term()}.
fold_keys(FoldKeysFun, Accum, Opts, State) ->
    KeyCount = State#state.key_count,
    BucketOpt = lists:keyfind(bucket, 1, Opts),
    Folder = case BucketOpt of
                 {bucket, Bucket} ->
                     FoldFun = fold_keys_fun(FoldKeysFun, Bucket),
                     get_folder(FoldFun, Accum, KeyCount);
                 _ ->
                     FoldFun = fold_keys_fun(FoldKeysFun, <<"all">>),
                     get_folder(FoldFun, Accum, KeyCount)
             end,
    case lists:member(async_fold, Opts) of
        true ->
            {async, Folder};
        false ->
            {ok, Folder()}
    end.

%% @doc Fold over all the objects for one or all buckets, yes, sir!
-spec fold_objects(legolas_backend:fold_objects_fun(),
                   any(),
                   [{atom(), term()}],
                   state()) -> {ok, any()} | {async, fun()}.
fold_objects(FoldObjectsFun, Accum, Opts, State) ->
    KeyCount = State#state.key_count,
    ValueSize = State#state.default_size,
    BucketOpt = lists:keyfind(bucket, 1, Opts),
    Folder = case BucketOpt of
                 {bucket, Bucket} ->
                     FoldFun = fold_objects_fun(FoldObjectsFun, Bucket, ValueSize),
                     get_folder(FoldFun, Accum, KeyCount);
                 _ ->
                     FoldFun = fold_objects_fun(FoldObjectsFun, <<"all">>, ValueSize),
                     get_folder(FoldFun, Accum, KeyCount)
             end,
    case lists:member(async_fold, Opts) of
        true ->
            {async, Folder};
        false ->
            {ok, Folder()}
    end.

%% @doc Delete all objects from this backend, yes, sir!
-spec drop(state()) -> {ok, state()}.
drop(S) ->
    {ok, S}.

%% @doc Returns true if this bitcasks backend contains any
%% non-tombstone values; otherwise returns false.
-spec is_empty(state()) -> false.
is_empty(_S) ->
    false.

-spec status(state()) -> [{atom(), term()}].
status(#state{op_put = Puts, op_get = Gets, op_delete = Deletes}) ->
    [{puts, Puts}, {gets, Gets}, {deletes, Deletes}].

%% @doc Register an asynchronous callback
-spec callback(reference(), any(), state()) -> {ok, state()}.
callback(_Ref, _Whatever, S) ->
    {ok, S}.


%% ===================================================================
%% Internal functions
%% ===================================================================

path_to_filename(Path, State) ->
    Filename = get_full_filename_in_partition(Path, State),
    ?DEBUG("Filename: ~p", [Filename]),
    Filename.

-spec do_put_data(string(), binary(), state()) -> ok | {error, term()}.
do_put_data(Path, Data, #state{partition_string=PartitionString} = State) ->
    ?DEBUG("do_put_data/2 Path: ~p Partition: ~p", [Path, PartitionString]),
    Filename = path_to_filename(Path, State),
    common_utils:write_file(Filename, Data).

-spec do_get_data(string(), state()) -> {ok, binary()} | {error, term()}.
do_get_data(Path, #state{partition_string=PartitionString} = State) ->
    ?DEBUG("do_get_data/2 Path: ~p Partition: ~p", [Path, PartitionString]),
    Filename = path_to_filename(Path, State),
    common_utils:read_file(Filename).

-spec do_delete_data(string(), state()) -> ok | {error, term()}.
do_delete_data(Path, #state{partition_string=PartitionString} = State) ->
    ?DEBUG("do_delete_data/2 Path: ~p Partition: ~p", [Path, PartitionString]),
    Filename = path_to_filename(Path, State),
    common_utils:delete_file(Filename).

get_folder(FoldFun, Acc, KeyCount) ->
    fun() ->
            fold_anything_fun(FoldFun, Acc, KeyCount)
    end.

key_of_integer(Range, State) ->
    {N, S} = random:uniform_s(Range, State),
    Key = common_utils:integer_to_fixed_list(N, 16, 160) ++ ".1000", %% e.g. "10.1000"
    BKey = list_to_binary(Key),          %% e.g. <<"10.1000">>
    {BKey, S}.

value_for_random(VR, Size) ->
    <<VR:(Size*8)>>.

fold_anything_fun(FoldFunc, Acc, KeyCount) ->
    Range = 1000000,
    KeyState = random:seed0(),
    ValueState = random:seed0(),
    all_keys_folder(FoldFunc, Acc, Range, {KeyState, ValueState}, KeyCount).

all_keys_folder(FoldFunc, Acc, _Range, _S, 0) ->
    FoldFunc(undefined, 0, Acc);
all_keys_folder(FoldFunc, Acc, Range, {KS,VS}, N) ->
    {Key,KSS} = key_of_integer(Range, KS),
    {VR,VSS} = random:uniform_s(255,VS),
    Acc1 = FoldFunc(Key, VR, Acc),
    all_keys_folder(FoldFunc, Acc1, Range, {KSS,VSS}, N-1).

%% @private
%% Return a function to fold over keys on this backend
fold_keys_fun(FoldKeysFun, Bucket) ->
    fun(Key, _VR, Acc) when Key /= undefined ->
            FoldKeysFun(Bucket, Key, Acc);
       (_, _, Acc) ->
            Acc
    end.

%% @private
%% Return a function to fold over keys on this backend
fold_objects_fun(FoldObjectsFun, Bucket, Size) ->
    fun(Key, VR, Acc) when Key /= undefined ->
            _Bin = value_for_random(VR, Size),
            %O = make_riak_safe_obj(Bucket, Key, Bin),
            %FoldObjectsFun(Bucket, Key, riak_object:to_binary(v0, O), Acc);
            FoldObjectsFun(Bucket, Key, <<>>, Acc);
       (_, _, Acc) ->
            Acc
    end.


