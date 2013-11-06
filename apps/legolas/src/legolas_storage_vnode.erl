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
         store_data/3,
         fetch_data/2,
         delete_data/2
        ]).

-record(state, {partition}).

-define(MASTER, legolas_storage_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

store_data(IdxNode, Path, Data) ->
    riak_core_vnode_master:sync_command(IdxNode,
                                   {store_data, Path, Data},
                                   ?MASTER).

fetch_data(IdxNode, Path) ->
    riak_core_vnode_master:sync_command(IdxNode,
                                        {fetch_data, Path},
                                        ?MASTER).
delete_data(IdxNode, Path) ->
    riak_core_vnode_master:sync_command(IdxNode,
                                   {delete_data, Path},
                                   ?MASTER).

%%%------------------------------------------------------------ 
%%% Callbacks
%%%------------------------------------------------------------ 

init([Partition]) ->
    {ok, #state { partition=Partition }}.

handle_command({store_data, Path, Data}, _Sender, #state{}=State) ->
    Result = do_store_data(Path, Data),
    {reply, Result, State};
handle_command({fetch_data, Path}, _Sender, #state{}=State) ->
    Result = do_fetch_data(Path),
    {reply, Result, State};
handle_command({delete_data, Path}, _Sender, #state{}=State) ->
    Result = do_delete_data(Path),
    {reply, Result, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

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

path_to_filename(Path) ->
    Filename = "data/" ++ binary_to_list(list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(Path))])),
    ?DEBUG("Filename: ~p", [Filename]),
    Filename.

do_store_data(Path, Data) ->
    ?DEBUG("do_store_data/2 Path: ~p Data: ~p", [Path, Data]),
    Filename = path_to_filename(Path),
    common_utils:write_file(legolas, Filename, Data).

do_fetch_data(Path) ->
    ?DEBUG("do_fetch_data/2 Path: ~p", [Path]),
    Filename = path_to_filename(Path),
    common_utils:read_file(legolas, Filename).

do_delete_data(Path) ->
    ?DEBUG("do_delete_data/2 Path: ~p", [Path]),
    Filename = path_to_filename(Path),
    common_utils:delete_file(legolas, Filename).

