%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-06 13:31:53
%%%------------------------------------------------------------ 

-module(legolas_app).
-include("legolas.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Start lager
    ok = lager:start(),
    %lager:trace_console([{module, legolas}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_app}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_cowboy_handler}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_storage_vnode}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_node_event_handler}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_ring_event_handler}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, common_utils}], debug),

    %% Start legolas cowboy 
    ok = legolas_cowboy_app:start(_StartType, _StartArgs),

    ?NOTICE("====================== Legolas Start ====================", []),

    case legolas_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(lagolas, [{vnode_module, legolas_vnode}]),
            ok = riak_core:register(lagolas, [{vnode_module, legolas_storage_vnode}]),

            ok = riak_core_ring_events:add_guarded_handler(legolas_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(legolas_node_event_handler, []),

            ok = riak_core_node_watcher:service_up(legolas, self()),
            ok = riak_core_node_watcher:service_up(legolas_storage, self()),

            {ok, Pid};

        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
