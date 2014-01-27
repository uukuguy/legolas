%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas调用接口。
%%%     
%%%     legolas操作由路径Path唯一指向的数据块Data。支持数据块级别的写入、读取、删除。
%%%
%%%     put_data - 写入数据块。
%%%     get_data - 读出数据块。
%%%     delete_data - 删除数据块。
%%%
%%% @end
%%% Created : 2014-01-14 02:13:49
%%%------------------------------------------------------------ 

-module(legolas).
-include("legolas.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("riak_core/include/riak_core_ring.hrl").

-define(TIMEOUT, 5000).

%% ------------------------------ Public APIs ------------------------------
-export([
         ping/0,
         put_data/2, %% (Path:string(), Data:string()) -> ok | {error, term()}
         put_data/3, %% (Path:string(), Data:string(), Options:list() | W:integer()) -> ok | {error, term()}
         get_data/1, %% (Path:string()) -> {ok, Data} | {error, term()}
         get_data/2, %% (Path:string(), Options:list() | R:integer()) -> {ok, Data} | {error, term()}
         delete_data/1, %% (Path:string()) -> ok | {error, term()}
         delete_data/2  %% (Path:string(), Options:list() | DW:integer()) -> ok | {error, term()}
        ]).

%% ------------------------------ Helper APIs ------------------------------
-export([
         to_object_key/2, %% (Bucket:binary(), Key:binary()) -> binary()
         from_object_key/1, %% (LKey:binary()) -> {Bucket:binary(), Key:binary()} | undefined
         to_index_key/4,
         from_index_key/1,
         to_first_key/1,
         get_chash_args/0,
         get_storage_preflist/2, %% (Path::string(), N:nag_integer()) -> riak_core_apl:preflist2()
         get_resource_hashkey/1,
         get_resource_primary_apl/1,
         get_resource_primary_apl/2,
         get_resource_apl/1,
         get_resource_apl/2,
         get_resource_vnode/1,
         preflist_siblings/1,
         responsible_preflists/1,
         get_index_n/1,
         %% 以下废弃
         path_to_filename/1
        ]).

%% ============================== Public APIs ==============================

%% ------------------------------ ping ------------------------------
%% @doc Pings a random vnode to make sure communication is functional
%%
ping() ->
    command(ping).

command(Cmd) ->
    CmdBin = list_to_binary(atom_to_list(Cmd)),
    DocIdx = riak_core_util:chash_key({CmdBin, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, legolas),
    [{IdxNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IdxNode, Cmd, legolas_vnode_master).

%% ------------------------------ put_data ------------------------------
%% @doc 写入由路径Path唯一指定的数据块Data。
%%      可以指定写参数，通常{w, 2}，表示完成2次成功写入才算成功。

-spec put_data(binary(), binary()) -> ok | {error, atom()}.
put_data(Path, Data) ->
    put_data(Path, Data, []).

-spec put_data(binary(), binary(), list() | integer()) -> ok | {error, term()}.
put_data(Path, Data, Options) when is_list(Options) ->
    ?NOTICE("call put_data/2", []),
    ?DEBUG("Enter put_data/2 Path = ~p ", [Path]),
    {ok, ReqId} = legolas_put_data_fsm:put_data(Path, Data),
    wait_for_reqid(ReqId, ?TIMEOUT);
put_data(Path, Data, W) ->
    put_data(Path, Data, [{w, W}]).

%% ------------------------------ get_data ------------------------------
%% @doc 读出由路径Path唯一指定的数据块Data。
%%      可以指定读参数，通常{r, 2}，表示完成2次成功读出才算成功。

-spec get_data(binary()) -> {ok, binary()} | {error, term()}.
get_data(Path) ->
    get_data(Path, []).

get_data(Path, Options) when is_list(Options) ->
    ?NOTICE("call get_data/1", []),
    ?DEBUG("Enter get_data/1 Path = ~p", [Path]),
    {ok, ReqId} = legolas_get_data_fsm:get_data(Path, Options),
    wait_for_reqid(ReqId, ?TIMEOUT);
get_data(Path, R) ->
    get_data(Path, [{r, R}]).

%% ------------------------------ delete_data ------------------------------
%% @doc 删除由路径Path唯一指定的数据块Data。
%%      可以指定删除参数，通常{dw, 2}，表示完成2次成功删除才算成功。

delete_data(Path) ->
    delete_data(Path, []).

delete_data(Path, Options) when is_list(Options) ->
    ?NOTICE("call delete_data/1", []),
    ?DEBUG("Enter delete_data/1 Path = ~p", [Path]),
    {ok, ReqId} = legolas_delete_data_fsm:delete_data(Path),
    wait_for_reqid(ReqId, ?TIMEOUT);
delete_data(Path, DW) ->
    delete_data(Path, [{dw, DW}]).


%% Private
%% 支持异步读写，等待所需成功完成的操作次数或超时返回。
wait_for_reqid(ReqId, Timeout) ->
    receive
        {ReqId, ok} -> ok;
        {ReqId, ok, Data} -> {ok, Data};
        {ReqId, error, Reason} -> {error, Reason}
    after Timeout ->
              {error, timeout}
    end.

%% ============================== Helper APIs ==============================

%% ------------------------------ to_object_key ------------------------------
%%

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

%% ------------------------------ to_index_key ------------------------------
%%

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

%% ------------------------------ to_first_key ------------------------------
%%

to_first_key(undefined) ->
    to_object_key(<<>>, <<>>);
to_first_key({bucket, Bucket}) ->
    to_object_key(Bucket, <<>>);
to_first_key(Other) ->
    erlang:throw({unknown_limiter, Other}).


%% ------------------------------ get_chash_args ------------------------------
%%

get_chash_args() ->
    N = common_utils:get_env(legolas, chash_N, ?DEFAULT_CHASH_N),
    R = common_utils:get_env(legolas, chash_R, ?DEFAULT_CHASH_R),
    W = common_utils:get_env(legolas, chash_W, ?DEFAULT_CHASH_W),
    DW = common_utils:get_env(legolas, chash_DW, ?DEFAULT_CHASH_DW),
    [{n, N}, {r, R}, {w, W}, {dw, DW}].

%% ------------------------------ get_storage_preflist ------------------------------
%% @doc 获取负责存储路径Path指向数据块的虚拟节点集合

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


%% ------------------------------ responsible_preflists ------------------------------
%% 被legolas_index_hashtree使用。

-type riak_core_ring() :: riak_core_ring:riak_core_ring().
-type index() :: non_neg_integer().
-type index_n() :: {index(), pos_integer()}.

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

%% Private
-spec responsible_preflists_n([index()], pos_integer()) -> [index_n()].
responsible_preflists_n(RevIndices, N) ->
    {Pred, _} = lists:split(N, RevIndices),
    [{Idx, N} || Idx <- lists:reverse(Pred)].


%% Private
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

%% ------------------------------ preflist_siblings ------------------------------
%% @doc Given an index, determine all sibling indices that participate in one
%%      or more preflists with the specified index.
-spec preflist_siblings(index()) -> [index()].
preflist_siblings(Index) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    preflist_siblings(Index, Ring).

%% @doc See {@link preflist_siblings/1}.
-spec preflist_siblings(index(), riak_core_ring()) -> [index()].
preflist_siblings(Index, Ring) ->
    MaxN = determine_max_n(Ring),
    preflist_siblings(Index, MaxN, Ring).

-spec preflist_siblings(index(), pos_integer(), riak_core_ring()) -> [index()].
preflist_siblings(Index, N, Ring) ->
    IndexBin = <<Index:160/integer>>,
    PL = riak_core_ring:preflist(IndexBin, Ring),
    Indices = [Idx || {Idx, _} <- PL],
    RevIndices = lists:reverse(Indices),
    {Succ, _} = lists:split(N-1, Indices),
    {Pred, _} = lists:split(N-1, tl(RevIndices)),
    lists:reverse(Pred) ++ Succ.

-spec determine_max_n(riak_core_ring()) -> pos_integer().
determine_max_n(Ring) ->
    lists:max(determine_all_n(Ring)).

%% ------------------------------ get_index_n ------------------------------
%% 被legolas_index_hashtree使用。
%% @doc Given a bucket/key, determine the associated preflist index_n.

-spec get_index_n({binary(), binary()}) -> index_n().
get_index_n({Bucket, Key}) ->
    BucketProps = riak_core_bucket:get_bucket(Bucket),
    N = proplists:get_value(n_val, BucketProps),
    ChashKey = riak_core_util:chash_key({Bucket, Key}),
    {ok, CHBin} = riak_core_ring_manager:get_chash_bin(),
    Index = chashbin:responsible_index(ChashKey, CHBin),
    {Index, N}.


%% 将路径Path转换成文件实际全路径。（已废弃！）
path_to_filename(Path) ->
    %Filename = "data/" ++ binary_to_list(list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(Path))])),
    Filename = "data/" ++ common_utils:binary_to_string(erlang:md5(Path)),
    ?DEBUG("Filename: ~p", [Filename]),
    Filename.


%% 以下函数仅供测试，目前未使用

%% ------------------------------ get_resource_hashkey ------------------------------
%% @doc 计算路径Path的哈希值。

get_resource_hashkey(Path) ->
    riak_core_util:chash_key({Path, Path}).


%% ------------------------------ get_resource_primary_apl ------------------------------
%%

get_resource_primary_apl(Path) ->
    get_resource_primary_apl(Path, 1).

get_resource_primary_apl(Path, N) ->
    DocIdx = get_resource_hashkey(Path),
    ?DEBUG("DocIdx : ~p", [DocIdx]),
    Preflist2 = riak_core_apl:get_primary_apl(DocIdx, N, legolas_storage),
    Preflist = [IndexNode || {IndexNode, _Type} <- Preflist2],
    Preflist.

%% ------------------------------ get_resource_apl ------------------------------
%%

get_resource_apl(Path) ->
    get_resource_apl(Path, 1).

get_resource_apl(Path, N) ->
    DocIdx = get_resource_hashkey(Path),
    ?DEBUG("DocIdx : ~p", [DocIdx]),
    riak_core_apl:get_apl(DocIdx, N, legolas_storage).

%% ------------------------------ get_resource_apl ------------------------------
%%

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


