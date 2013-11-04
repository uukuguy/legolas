-module(legolas_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case legolas_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, legolas_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(legolas_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(legolas_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(legolas, self()),

            %% Start lager
            ok = lager:start(),

            %% Start legolas cowboy 
            ok = legolas_cowboy_app:start(_StartType, _StartArgs),

            {ok, Pid};

        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
