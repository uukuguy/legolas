-module(legolas).
-include("legolas.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         store_data/2
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

store_data(Path, Data) ->
    ?DEBUG({"Enter store_data", " Path = ", Path}),
    %DocIdx = riak_core_util:chash_key({list_to_binary(Path), term_to_binary(now())}),
    DocIdx = riak_core_util:chash_key({<<Path/binary>>, term_to_binary(now())}),
    ?DEBUG({"DocIdx = ", DocIdx}),
    PrefList = riak_core_apl:get_apl(DocIdx, 1, legolas_storage),
    ?DEBUG({"PrefList :", PrefList}),
    [IdxNode] = PrefList,
    ?DEBUG({"IdxNode : ", IdxNode}),
    legolas_store_vnode:store_data(IdxNode, Path, Data).
