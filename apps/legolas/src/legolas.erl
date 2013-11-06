-module(legolas).
%-compile([{parse_transform, lager_transform}]).
-include("legolas.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         store_data/2,
         fetch_data/1,
         delete_data/1
        ]).

-export([
         get_resource_hashkey/1,
         get_resource_primary_apl/1,
         get_resource_apl/1,
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

get_resource_hashkey(Path) ->
    riak_core_util:chash_key({Path, Path}).

get_resource_primary_apl(Path) ->
    DocIdx = get_resource_hashkey(Path),
    ?DEBUG("DocIdx : ~p", [DocIdx]),
    riak_core_apl:get_primary_apl(DocIdx, 1, legolas_storage).

get_resource_apl(Path) ->
    DocIdx = get_resource_hashkey(Path),
    ?DEBUG("DocIdx : ~p", [DocIdx]),
    riak_core_apl:get_apl(DocIdx, 1, legolas_storage).

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

store_data(Path, Data) ->
    ?NOTICE("-------------------- call store_data/2 --------------------", []),
    ?DEBUG("Enter store_data Path = ~p ", [Path]),
    case get_resource_vnode(Path) of
        not_found -> {error, "vnode not found."};
        IdxNode -> legolas_storage_vnode:store_data(IdxNode, Path, Data)
    end.

fetch_data(Path) ->
    ?NOTICE("-------------------- call fetch_data/1 --------------------", []),
    ?DEBUG("Enter store_data Path = ~p", [Path]),
    case get_resource_vnode(Path) of
        not_found -> {error, "vnode not found."};
        IdxNode -> legolas_storage_vnode:fetch_data(IdxNode, Path)
    end.

delete_data(Path) ->
    ?NOTICE("-------------------- call delete_data/1 --------------------", []),
    ?DEBUG("Enter delete_data Path = ~p", [Path]),
    case get_resource_vnode(Path) of
        not_found -> {error, "vnode not found."};
        IdxNode -> legolas_storage_vnode:delete_data(IdxNode, Path)
    end.
