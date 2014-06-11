%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas辅助虚拟节点。目前提供ping/0函数。
%%%
%%% @end
%%% Created : 2013-11-04 21:44:12
%%%------------------------------------------------------------ 

-module(legolas_vnode).
-behaviour(riak_core_vnode).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
         start_vnode/1
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
         handle_exit/3
        ]).

%% ------------------------------ record ------------------------------ 
-record(state, {partition}).

%% ============================== APIs ==============================
%%

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).


%% ============================== Callbacks ==============================
%%

init([Partition]) ->
    {ok, #state { partition=Partition }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};
handle_command(Message, _Sender, State) ->
    ?NOTICE("Unhandled commad : ", [Message]),
    {noreply, State}.

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


