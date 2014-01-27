%%%------------------------------------------------------------------------------------------------
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc legolas文件块后端。用leveldb保存索引信息，文件内容保存在64M大小的文件块中。
%%%
%%% @end
%%% Created : 2013-11-24 20:12:24
%%%------------------------------------------------------------------------------------------------ 

-module(legolas_fileblock_backend).
-include("legolas.hrl").
-behavior(legolas_backend).

%% Legolas Storage Backend API
-export([api_version/0,
         get_partition_string/1,
         capabilities/1,
         capabilities/2,
         data_size/1,
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
-define(CAPABILITIES, [
                       async_fold, 
                       size
                      ]).

-record(state, {
          partition,
          partition_datadir, %% string()
          partition_string, %% string()
          default_size = 0,
          key_count = 0,
          ref = undefined :: reference(),
          indexes_datadir :: string(),
          open_opts = [],
          read_opts = [],
          write_opts = [],
          fold_opts = [],
          config = [],
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

make_partition_string(Partition) ->
    PartitionString = common_utils:integer_to_fixed_list(Partition, 16, 160),
    PartitionString.

get_root_datadir() ->
    %RootDataDir = code:priv_dir(legolas) ++ "/data",
    RootDataDir = app_helper:get_env(legolas, storage_datadir),
    RootDataDir.

%get_partition_datadir(Partition) ->
    %RootDataDir = get_root_datadir(),
    %PartitionString = make_partition_string(Partition),
    %DataDir =  RootDataDir ++ "/" ++ PartitionString,
    %DataDir.

get_partition_string(#state{partition_string=PartitionString}) ->
    PartitionString.
                      
get_full_filename_in_partition(Path, #state{partition_datadir=DataDir}) ->
    Filename = DataDir ++ "/" ++ common_utils:binary_to_string(erlang:md5(Path)),
    Filename.

%% @doc Start this backend, yes, sir!
-spec start(integer(), config()) -> {ok, state()} | {error, term()}.
start(Partition, Config) ->
    %?DEBUG("Starting fileblock backend. Partition: ~p", [integer_to_list(Partition, 16)]),
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
    PartitionString = make_partition_string(Partition),
    DataDir = RootDataDir ++ "/" ++ PartitionString,
    Result = case filelib:ensure_dir(filename:join(DataDir, "dummy")) of
                 ok ->
                     IndexesDataDir = DataDir ++ "/indexes",
                     IndexesState = init_indexes_state(IndexesDataDir, Config),
                     case open_db(IndexesState) of
                         {ok, State0} ->
                             {ok, State0#state{
                                    partition = Partition,
                                    partition_datadir = DataDir,
                                    partition_string = PartitionString,
                                    default_size = DefaultLen,
                                    key_count = KeyCount,
                                    bprefix_list = BPrefixList
                                   }};
                         {error, Reason} ->
                             {error, Reason}
                     end;
                 {error, Reason} ->
                     ?ERROR("Error in ensure_datadir/1! Reason: ~p Path: ~p", [Reason, DataDir]),
                     {error, Reason}
             end,
    Result.

%% @doc Stop this backend!
-spec stop(state()) -> ok.
stop(#state{partition_string=PartitionString} = State) ->
    ?DEBUG("Backend stop! close eleveldb. Partition: ~p", [PartitionString]),
    case State#state.ref of
        undefined ->
            ok;
        _ ->
            eleveldb:close(State#state.ref)
    end,
    ok.

%% @doc Get a fake object, yes, sir!
-spec get(riak_object:bucket(), riak_object:key(), state()) ->
                 {ok, any(), state()} | {error, term()}.
get(Bucket, Key, #state{
                    ref=Ref,
                    read_opts=ReadOpts
                   } = State) ->
    ?DEBUG("Ref: ~p Bucket: ~p Key: ~p", [Ref, Bucket, Key]),
    StorageKey = legolas:to_object_key(Bucket, Key),
    Result = case eleveldb:get(Ref, StorageKey, ReadOpts) of
                 {ok, _} ->
                     get_object(Bucket, Key, true, State);
                 not_found  ->
                     {error, not_found, State};
                 {error, Reason} ->
                     {error, Reason, State}
             end,
    Result.

get_object(Bucket, Key, WantsBinary, S) ->
    get_object_bprefix(S#state.bprefix_list, Bucket, Key, WantsBinary, S).

get_object_bprefix([], _Bucket, Key, _WantsBinary, #state{op_get = Gets} = S) ->
    case do_get_data(Key, S) of
        {ok, Data} ->
            {ok, Data, S#state{op_get = Gets + 1} };
        {error, Reason} ->
            {error, Reason, S}
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
                 {ok, state()} | {error, term(), state()}.
put(Bucket, PrimaryKey, IndexSpecs, Val, #state{
                                             ref=Ref,
                                             write_opts=WriteOpts,
                                             op_put=Puts
                                            } = State) ->
    case do_put_data(PrimaryKey, Val, State) of
        ok -> 
            StorageKey = legolas:to_object_key(Bucket, PrimaryKey),
            PutUpdates = [{put, StorageKey, Val}],

            F = fun({add, Field, Value}) ->
                        [{put, legolas:to_index_key(Bucket, PrimaryKey, Field, Value), <<>>}];
                   ({remove, Field, Value}) ->
                        [{delete, legolas:to_index_key(Bucket, PrimaryKey, Field, Value)}]
                end,
            IndexUpdates = lists:flatmap(F, IndexSpecs),
            
            IndexResult = case eleveldb:write(Ref, PutUpdates ++ IndexUpdates, WriteOpts) of
                              ok ->
                                  {ok, State#state{op_put = Puts + 1}};
                              {error, Reason} ->
                                  {error, Reason, State}
                          end,
            IndexResult;
        {error, Reason} -> {error, Reason, State}
    end.

%% @doc Delete an object, yes, sir!
-spec delete(riak_object:bucket(), riak_object:key(), [index_spec()], state()) ->
                    {ok, state()} | {error, term()}.
delete(Bucket, PrimaryKey, IndexSpecs, #state{
                                           ref=Ref,
                                           write_opts=WriteOpts,
                                           op_delete = Deletes
                                          } = State) ->
    StorageKey = legolas:to_object_key(Bucket, PrimaryKey),
    DeleteUpdates = [{delete, StorageKey}],

    F = fun({remove, Field, Value}) ->
                [{delete, legolas:to_index_key(Bucket, PrimaryKey, Field, Value)}]
        end,
    IndexUpdates = lists:flatmap(F, IndexSpecs),

    case eleveldb:write(Ref, DeleteUpdates ++ IndexUpdates, WriteOpts) of
        ok ->
            case do_delete_data(PrimaryKey, State) of
                ok -> {ok, State#state{op_delete = Deletes + 1}};
                {error, Reason} -> {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
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
fold_keys(FoldKeysFun, Accum, Opts, #state{partition_string=PartitionString}=State) ->
    ?DEBUG("fold_keys/4 Partition: ~p", [PartitionString]),
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

%% @doc Fold over all the objects for one or all buckets.
-spec fold_objects(legolas_backend:fold_objects_fun(),
                   any(),
                   [{atom(), term()}],
                   state()) -> {ok, any()} | {async, fun()}.
fold_objects(FoldObjectsFun, Acc, Opts, #state{
                                           fold_opts=FoldOpts
                                          } = State) ->
    %% Figure out how we should limit the fold: by bucket, by
    %% secondary index, or neither (fold across everything.)
    Bucket = lists:keyfind(bucket, 1, Opts),
    Index = lists:keyfind(index, 1, Opts),

    %% Multiple limiters may exist. Take the most specific limiter.
    Limiter =
        if Index /= false  -> Index;
           Bucket /= false -> Bucket;
           true            -> undefined
        end,

    %% Set up the fold...
    FirstKey = legolas:to_first_key(Limiter),
    FoldOpts1 = [{first_key, FirstKey} | FoldOpts],
    FoldFun = fold_objects_fun(FoldObjectsFun, Limiter, State),

    ObjectFolder = get_object_folder(FoldFun, Acc, FoldOpts1, State),

    case lists:member(async_fold, Opts) of
        true ->
            {async, ObjectFolder};
        false ->
            {ok, ObjectFolder()}
    end.

get_object_folder(FoldFun, Acc, FoldOpts, #state{ref=Ref}) ->
    %fun() ->
            %fold_anything_fun(FoldFun, Acc, FoldOpts, State)
    %end.
    fun() ->
            try
                ?DEBUG("try eleveldb:fold/4 Ref: ~p FoldFun: ~p Acc: ~p", [Ref, FoldFun, Acc]),
                eleveldb:fold(Ref, FoldFun, Acc, FoldOpts)
            catch
                {break, AccFinal} ->
                    ?ERROR("eleveldb:fold/4 break, AccFinal: ~p", [AccFinal]),
                    AccFinal
            end
    end.

%fold_anything_fun(FoldFun, Acc, _FoldOpts, #state{partition_string=PartitionString} = _State) ->
    %?DEBUG("Start to fold all objects. Partition: ~p", [PartitionString]),
    %Bucket = <<>>,
    %Key = "/a1/b1/c1",
    %StorageKey = legolas:to_object_key(Bucket, Key),
    %Value = <<"">>,
    %FoldFun({StorageKey, Value}, Acc).


fold_objects_fun(FoldObjectsFun, {bucket, FilterBucket}, State) ->
    fun({StorageKey, _Value}, Acc) ->
            ?DEBUG("StorageKey: ~p", [StorageKey]),
            case legolas:from_object_key(StorageKey) of
                {Bucket, Key} when Bucket == FilterBucket ->
                    ?DEBUG("Bucket: ~p Key: ~p", [Bucket, Key]),
                    case do_get_data(Key, State) of
                        {ok, Data} ->
                            FoldObjectsFun(Bucket, Key, Data, Acc);
                        {error, _Reason} ->
                            throw({break, Acc})
                    end;
                ignore ->
                    Acc;
                _ ->
                    throw({break, Acc})
            end
    end;
fold_objects_fun(FoldObjectsFun, undefined, State) ->
    fun({StorageKey, _Value}, Acc) ->
            ?DEBUG("StorageKey: ~p", [StorageKey]),
            case legolas:from_object_key(StorageKey) of
                {Bucket, Key} ->
                    ?DEBUG("Bucket: ~p Key: ~p", [Bucket, Key]),
                    case do_get_data(Key, State) of
                        {ok, Data} ->
                            FoldObjectsFun(Bucket, Key, Data, Acc);
                        {error, _Reason} ->
                            throw({break, Acc})
                    end;
                ignore ->
                    Acc;
                _ ->
                    throw({break, Acc})
            end
    end.


%% @doc Fold over all the objects for one or all buckets, yes, sir!
%-spec fold_objects(legolas_backend:fold_objects_fun(),
                   %any(),
                   %[{atom(), term()}],
                   %state()) -> {ok, any()} | {async, fun()}.
%fold_objects(FoldObjectsFun, Accum, Opts, #state{
                                             %fold_opts=FoldOpts,
                                             %partition_string=PartitionString
                                            %}=State) ->
    %?DEBUG("fold_objects/4 FoldObjectsFun: ~p Accum: ~p Partition: ~p", [FoldObjectsFun, Accum, PartitionString]),
    %KeyCount = State#state.key_count,
    %ValueSize = State#state.default_size,
    %BucketOpt = lists:keyfind(bucket, 1, Opts),
    %Folder = case BucketOpt of
                 %{bucket, Bucket} ->
                     %FoldFun = fold_objects_fun(FoldObjectsFun, Bucket, ValueSize),
                     %get_folder(FoldFun, Accum, KeyCount);
                 %_ ->
                     %FoldFun = fold_objects_fun(FoldObjectsFun, <<"all">>, ValueSize),
                     %get_folder(FoldFun, Accum, KeyCount)
             %end,
    %case lists:member(async_fold, Opts) of
        %true ->
            %{async, Folder};
        %false ->
            %{ok, Folder()}
    %end.


%% @private
%% Return a function to fold over keys on this backend
%fold_objects_fun(FoldObjectsFun, Bucket, Size) ->
    %fun(Key, VR, Acc) when Key /= undefined ->
            %_Bin = value_for_random(VR, Size),
            %%O = make_riak_safe_obj(Bucket, Key, Bin),
            %%FoldObjectsFun(Bucket, Key, riak_object:to_binary(v0, O), Acc);
            %FoldObjectsFun(Bucket, Key, <<>>, Acc);
       %(_, _, Acc) ->
            %Acc
    %end.

%% @doc Delete all objects from this backend, yes, sir!
-spec drop(state()) -> {ok, state()}.
drop(S) ->
    {ok, S}.

%% @doc Returns true if this bitcasks backend contains any
%% non-tombstone values; otherwise returns false.
-spec is_empty(state()) -> boolean().
is_empty(#state{ref=Ref, read_opts=_ReadOpts, write_opts=_WriteOpts}) ->
    case eleveldb:is_empty(Ref) of
        true ->
            true;
        false ->
            %is_empty_but_md(Ref, ReadOpts, WriteOpts)
            false
    end.

-spec data_size(state()) -> undefined | {non_neg_integer(), bytes}.
data_size(_State) ->
    {0, bytes}.
    %try {ok, <<SizeStr/binary>>} = eleveldb:status(State#state.ref, <<"leveldb.total-bytes">>),
         %list_to_integer(binary_to_list(SizeStr)) of
        %Size -> {Size, bytes}
    %catch
        %error:_ -> undefined
    %end.

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

%value_for_random(VR, Size) ->
    %<<VR:(Size*8)>>.

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
init_indexes_state(IndexesDataDir, Config) ->
    %% Get the data root directory
    filelib:ensure_dir(filename:join(IndexesDataDir, "dummy")),

    %% Merge the proplist passed in from Config with any values specified by the
    %% eleveldb app level; precedence is given to the Config.
    MergedConfig = orddict:merge(fun(_K, VLocal, _VGlobal) -> VLocal end,
                                 orddict:from_list(Config), % Local
                                 orddict:from_list(application:get_all_env(eleveldb))), % Global

    %% Use a variable write buffer size in order to reduce the number
    %% of vnodes that try to kick off compaction at the same time
    %% under heavy uniform load...
    WriteBufferMin = common_utils:get_config_value(write_buffer_size_min, MergedConfig, 30 * 1024 * 1024),
    WriteBufferMax = common_utils:get_config_value(write_buffer_size_max, MergedConfig, 60 * 1024 * 1024),
    WriteBufferSize = WriteBufferMin + random:uniform(1 + WriteBufferMax - WriteBufferMin),

    %% Update the write buffer size in the merged config and make sure create_if_missing is set
    %% to true
    FinalConfig = orddict:store(write_buffer_size, WriteBufferSize,
                                orddict:store(create_if_missing, true, MergedConfig)),

    %% Parse out the open/read/write options
    {OpenOpts, _BadOpenOpts} = eleveldb:validate_options(open, FinalConfig),
    {ReadOpts, _BadReadOpts} = eleveldb:validate_options(read, FinalConfig),
    {WriteOpts, _BadWriteOpts} = eleveldb:validate_options(write, FinalConfig),

    %% Use read options for folding, but FORCE fill_cache to false
    FoldOpts = lists:keystore(fill_cache, 1, ReadOpts, {fill_cache, false}),

    %% Warn if block_size is set
    SSTBS = proplists:get_value(sst_block_size, OpenOpts, false),
    BS = proplists:get_value(block_size, OpenOpts, false),
    case BS /= false andalso SSTBS == false of
        true ->
            lager:warning("eleveldb block_size has been renamed sst_block_size "
                          "and the current setting of ~p is being ignored.  "
                          "Changing sst_block_size is strongly cautioned "
                          "against unless you know what you are doing.  Remove "
                          "block_size from app.config to get rid of this "
                          "message.\n", [BS]);
        _ ->
            ok
    end,

    %% Generate a debug message with the options we'll use for each operation
    %?DEBUG("IndexesDataDir ~s options for LevelDB: ~p\n",
                %[IndexesDataDir, [{open, OpenOpts}, {read, ReadOpts}, {write, WriteOpts}, {fold, FoldOpts}]]),
    #state { indexes_datadir = IndexesDataDir,
             open_opts = OpenOpts,
             read_opts = ReadOpts,
             write_opts = WriteOpts,
             fold_opts = FoldOpts,
             config = FinalConfig }.

-spec open_db(state()) -> {ok, state()} | {error, term()}.
open_db(#state{ref=Ref} = State) ->
    case Ref of
        undefined ->
            RetriesLeft = app_helper:get_env(legolas, eleveldb_open_retries, 3),
            open_db(State, max(1, RetriesLeft), undefined);
        _ ->
            {ok, State}
    end.

open_db(_State0, 0, LastError) ->
    {error, LastError};
open_db(State0, RetriesLeft, _) ->
    %{ok, State0}.
    case eleveldb:open(State0#state.indexes_datadir, State0#state.open_opts) of
        {ok, Ref} ->
            {ok, State0#state { ref = Ref }};
        %% Check specifically for lock error, this can be caused if
        %% a crashed vnode takes some time to flush leveldb information
        %% out to disk.  The process is gone, but the NIF resource cleanup
        %% may not have completed.
        {error, {db_open, OpenErr}=Reason} ->
            case lists:prefix("IO error: lock ", OpenErr) of
                true ->
                    SleepFor = app_helper:get_env(legolas, eleveldb_open_retry_delay, 2000),
                    ?WARNING("Leveldb backend retrying ~p in ~p ms after error ~s\n",
                                [State0#state.indexes_datadir, SleepFor, OpenErr]),
                    timer:sleep(SleepFor),
                    open_db(State0, RetriesLeft - 1, Reason);
                false ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


