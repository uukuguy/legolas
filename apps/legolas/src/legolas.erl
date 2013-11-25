-module(legolas).
%-compile([{parse_transform, lager_transform}]).
-include("legolas.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-define(TIMEOUT, 5000).

-export([
         ping/0,
         put_data/2,
         get_data/1,
         delete_data/1
        ]).

-export([
         path_to_filename/1,
         get_chash_args/0,
         get_storage_preflist/2, %% (Path::string(), N:nag_integer()) -> riak_core_apl:preflist2()
         get_resource_hashkey/1,
         get_resource_primary_apl/1,
         get_resource_primary_apl/2,
         get_resource_apl/1,
         get_resource_apl/2,
         get_resource_vnode/1
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
    ?NOTICE("-------------------- call put_data/2 --------------------", []),
    ?DEBUG("Enter put_data/2 Path = ~p ", [Path]),
    %case get_resource_vnode(Path) of
        %not_found -> {error, "vnode not found."};
        %IdxNode -> legolas_storage_vnode:put_data(IdxNode, Path, Data)
    %end.
    {ok, ReqId} = legolas_put_data_fsm:put_data(Path, Data),
    wait_for_reqid(ReqId, ?TIMEOUT).

get_data(Path) ->
    ?NOTICE("-------------------- call get_data/1 --------------------", []),
    ?DEBUG("Enter get_data/1 Path = ~p", [Path]),
    %case get_resource_vnode(Path) of
        %not_found -> {error, "vnode not found."};
        %IdxNode -> legolas_storage_vnode:get_data(IdxNode, Path)
    %end.
    {ok, ReqId} = legolas_get_data_fsm:get_data(Path),
    wait_for_reqid(ReqId, ?TIMEOUT).

delete_data(Path) ->
    ?NOTICE("-------------------- call delete_data/1 --------------------", []),
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

