-module(legolas).
%-compile([{parse_transform, lager_transform}]).
-include("legolas.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("riak_core/include/riak_core_ring.hrl").

-define(TIMEOUT, 5000).

-export([
         ping/0,
         put_data/2,
         get_data/1,
         delete_data/1
        ]).

-export([
         to_object_key/2,
         from_object_key/1,
         to_index_key/4,
         from_index_key/1,
         to_first_key/1,
         path_to_filename/1,
         get_chash_args/0,
         get_storage_preflist/2, %% (Path::string(), N:nag_integer()) -> riak_core_apl:preflist2()
         get_resource_hashkey/1,
         get_resource_primary_apl/1,
         get_resource_primary_apl/2,
         get_resource_apl/1,
         get_resource_apl/2,
         get_resource_vnode/1,
         responsible_preflists/1,
         get_index_n/1
        ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    command(ping).
    %DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    %PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, legolas),
    %[{IdxNode, _Type}] = PrefList,
    %riak_core_vnode_master:sync_spawn_command(IdxNode, ping, legolas_vnode_master).


command(Cmd) ->
    CmdBin = list_to_binary(atom_to_list(Cmd)),
    DocIdx = riak_core_util:chash_key({CmdBin, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, legolas),
    [{IdxNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IdxNode, Cmd, legolas_vnode_master).

path_to_filename(Path) ->
    %Filename = "data/" ++ binary_to_list(list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(Path))])),
    Filename = "data/" ++ common_utils:binary_to_string(erlang:md5(Path)),
    ?DEBUG("Filename: ~p", [Filename]),
    Filename.

get_chash_args() ->
    N = common_utils:get_env(legolas, chash_N, ?DEFAULT_CHASH_N),
    R = common_utils:get_env(legolas, chash_N, ?DEFAULT_CHASH_R),
    W = common_utils:get_env(legolas, chash_N, ?DEFAULT_CHASH_W),
    {N, R, W}.

%% -type preflist2() :: [{{index(), node()}, primary|fallback}].
-spec get_storage_preflist(string(), pos_integer()) -> riak_core_apl:preflist2().
get_storage_preflist(Path, N) ->
    Preflist = get_resource_preflist(legolas_storage, {<<>>, Path}, N),
    Preflist.

-spec get_resource_preflist(atom(), term(), pos_integer()) -> riak_core_apl:preflist2().
get_resource_preflist(Service, Resource, N) ->
    ResourceIdx = riak_core_util:chash_key(Resource),
    ?DEBUG("ResourceIdx : ~p", [ResourceIdx]),
    %PrefList = riak_core_apl:get_apl(ResourceIdx, N, Service),
    %Preflist2 = riak_core_apl:get_primary_apl(ResourceIdx, N, Service),
    UpNodes = riak_core_node_watcher:nodes(Service),
    ?DEBUG("UpNodes: ~p", [UpNodes]),
    Preflist2 = riak_core_apl:get_apl_ann(ResourceIdx, N, UpNodes),
    Preflist = [IndexNode || {IndexNode, _Type} <- Preflist2],
    ?DEBUG("Preflist : ~p", [Preflist]),
    Preflist.

get_resource_hashkey(Path) ->
    riak_core_util:chash_key({Path, Path}).

get_resource_primary_apl(Path) ->
    get_resource_primary_apl(Path, 1).

get_resource_primary_apl(Path, N) ->
    DocIdx = get_resource_hashkey(Path),
    ?DEBUG("DocIdx : ~p", [DocIdx]),
    Preflist2 = riak_core_apl:get_primary_apl(DocIdx, N, legolas_storage),
    Preflist = [IndexNode || {IndexNode, _Type} <- Preflist2],
    Preflist.

get_resource_apl(Path) ->
    get_resource_apl(Path, 1).

get_resource_apl(Path, N) ->
    DocIdx = get_resource_hashkey(Path),
    ?DEBUG("DocIdx : ~p", [DocIdx]),
    riak_core_apl:get_apl(DocIdx, N, legolas_storage).

get_resource_vnode(Path) ->
    case get_resource_apl(Path) of
        [] -> ?WARNING("PrefList = [], vnode not found! Path = ~p", [Path]),
              not_found;
        PrefList -> 
            ?DEBUG("PrefList : ~p", [PrefList]),
            [IdxNode] = PrefList,
            ?DEBUG("IdxNode : ~p", [IdxNode]),
            IdxNode %% {Partition, Node}
    end.

put_data(Path, Data) ->
    ?NOTICE("call put_data/2", []),
    ?DEBUG("Enter put_data/2 Path = ~p ", [Path]),
    %case get_resource_vnode(Path) of
        %not_found -> {error, "vnode not found."};
        %IdxNode -> legolas_storage_vnode:put_data(IdxNode, Path, Data)
    %end.
    {ok, ReqId} = legolas_put_data_fsm:put_data(Path, Data),
    wait_for_reqid(ReqId, ?TIMEOUT).

get_data(Path) ->
    ?NOTICE("call get_data/1", []),
    ?DEBUG("Enter get_data/1 Path = ~p", [Path]),
    %case get_resource_vnode(Path) of
        %not_found -> {error, "vnode not found."};
        %IdxNode -> legolas_storage_vnode:get_data(IdxNode, Path)
    %end.
    {ok, ReqId} = legolas_get_data_fsm:get_data(Path),
    wait_for_reqid(ReqId, ?TIMEOUT).

delete_data(Path) ->
    ?NOTICE("call delete_data/1", []),
    ?DEBUG("Enter delete_data/1 Path = ~p", [Path]),
    %case get_resource_vnode(Path) of
        %not_found -> {error, "vnode not found."};
        %IdxNode -> legolas_storage_vnode:delete_data(IdxNode, Path)
    %end.
    {ok, ReqId} = legolas_delete_data_fsm:delete_data(Path),
    wait_for_reqid(ReqId, ?TIMEOUT).

wait_for_reqid(ReqId, Timeout) ->
    receive
        {ReqId, ok} -> ok;
        {ReqId, ok, Data} -> {ok, Data};
        {ReqId, error, Reason} -> {error, Reason}
    after Timeout ->
              {error, timeout}
    end.

to_object_key(Bucket, Key) ->
    sext:encode({o, Bucket, Key}).

from_object_key(LKey) ->
    case (catch sext:decode(LKey)) of
        {'EXIT', _} -> 
            lager:warning("Corrupted object key, discarding"),
            ignore;
        {o, Bucket, Key} ->
            {Bucket, Key};
        _ ->
            undefined
    end.


to_index_key(Bucket, Key, Field, Term) ->
    sext:encode({i, Bucket, Field, Term, Key}).

from_index_key(LKey) ->
    case (catch sext:decode(LKey)) of
        {'EXIT', _} -> 
            ?WARNING("Corrupted index key, discarding", []),
            ignore;
        {i, Bucket, Field, Term, Key} ->
            {Bucket, Key, Field, Term};
        _ ->
            undefined
    end.


%% @private Given a scope limiter, use sext to encode an expression
%% that represents the starting key for the scope. For example, since
%% we store objects under {o, Bucket, Key}, the first key for the
%% bucket "foo" would be `sext:encode({o, <<"foo">>, <<>>}).`
to_first_key(undefined) ->
    %% Start at the first object in LevelDB...
    to_object_key(<<>>, <<>>);
to_first_key({bucket, Bucket}) ->
    %% Start at the first object for a given bucket...
    to_object_key(Bucket, <<>>).
%to_first_key({index, incorrect_format, ForUpgrade}) when is_boolean(ForUpgrade) ->
    %%% Start at first index entry
    %to_index_key(<<>>, <<>>, <<>>, <<>>);
%%% V2 indexes
%to_first_key({index, Bucket,
              %?KV_INDEX_Q{filter_field=Field,
                          %start_key=StartKey}}) when Field == <<"$key">>;
                                                     %Field == <<"$bucket">> ->
    %to_object_key(Bucket, StartKey);
%to_first_key({index, Bucket, ?KV_INDEX_Q{filter_field=Field,
                                         %start_key=StartKey,
                                         %start_term=StartTerm}}) ->
    %to_index_key(Bucket, StartKey, Field, StartTerm);

%%% Upgrade legacy queries to current version
%to_first_key({index, Bucket, Q}) ->
    %UpgradeQ = riak_index:upgrade_query(Q),
    %to_first_key({index, Bucket, UpgradeQ});
%to_first_key(Other) ->
    %erlang:throw({unknown_limiter, Other}).

-type riak_core_ring() :: riak_core_ring:riak_core_ring().
-type index() :: non_neg_integer().
-type index_n() :: {index(), pos_integer()}.


-spec determine_all_n(riak_core_ring()) -> [pos_integer(),...].
determine_all_n(Ring) ->
    Buckets = riak_core_ring:get_buckets(Ring),
    BucketProps = [riak_core_bucket:get_bucket(Bucket, Ring) || Bucket <- Buckets],
    Default = app_helper:get_env(riak_core, default_bucket_props),
    DefaultN = proplists:get_value(n_val, Default),
    AllN = lists:foldl(fun(Props, AllN) ->
                               N = proplists:get_value(n_val, Props),
                               ordsets:add_element(N, AllN)
                       end, [DefaultN], BucketProps),
    AllN.

-spec responsible_preflists(index()) -> [index_n()].
responsible_preflists(Index) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    responsible_preflists(Index, Ring).

-spec responsible_preflists(index(), riak_core_ring()) -> [index_n()].
responsible_preflists(Index, Ring) ->
    AllN = determine_all_n(Ring),
    responsible_preflists(Index, AllN, Ring).

-spec responsible_preflists(index(), [pos_integer(),...], riak_core_ring())
                           -> [index_n()].
responsible_preflists(Index, AllN, Ring) ->
    IndexBin = <<Index:160/integer>>,
    PL = riak_core_ring:preflist(IndexBin, Ring),
    Indices = [Idx || {Idx, _} <- PL],
    RevIndices = lists:reverse(Indices),
    lists:flatmap(fun(N) ->
                          responsible_preflists_n(RevIndices, N)
                  end, AllN).

-spec responsible_preflists_n([index()], pos_integer()) -> [index_n()].
responsible_preflists_n(RevIndices, N) ->
    {Pred, _} = lists:split(N, RevIndices),
    [{Idx, N} || Idx <- lists:reverse(Pred)].


%% @doc Given a bucket/key, determine the associated preflist index_n.
-spec get_index_n({binary(), binary()}) -> index_n().
get_index_n({Bucket, Key}) ->
    BucketProps = riak_core_bucket:get_bucket(Bucket),
    N = proplists:get_value(n_val, BucketProps),
    ChashKey = riak_core_util:chash_key({Bucket, Key}),
    {ok, CHBin} = riak_core_ring_manager:get_chash_bin(),
    Index = chashbin:responsible_index(ChashKey, CHBin),
    {Index, N}.

