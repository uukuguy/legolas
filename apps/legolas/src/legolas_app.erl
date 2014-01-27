%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     应用程序启动入口。
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

    ok = riak_core_util:start_app_deps(legolas),

    %% -------------------- lager --------------------

    % 将核心模块相关的日志记录到log/legolas_debug.log中，方便调试。
    lager:trace_file("log/legolas_debug.log", [{module, legolas}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_app}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_cowboy_handler}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_storage_vnode}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_fileblock_backend}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_put_data_fsm}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_get_data_fsm}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_delete_data_fsm}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_node_event_handler}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, legolas_ring_event_handler}], debug),
    lager:trace_file("log/legolas_debug.log", [{module, common_utils}], debug),

    % from legolas_entropy_manager:set_debug.
    %lager:trace_console([{module, legolas}], debug),
    %common_utils:enable_console_debug(false, [
                                              %riak_core_vnode_manager,
                                              %riak_core_vnode
                                             %]),
    %common_utils:enable_console_debug(true, []),

    %% -------------------- cowboy --------------------
    ok = legolas_cowboy_app:start(_StartType, _StartArgs),

    ?NOTICE("=== Legolas Start ===", []),

    %% -------------------- riak_core --------------------
    %% 注册legolas、legolas_storage两个服务。
    %%
    case legolas_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(legolas, [{vnode_module, legolas_vnode}]),
            ok = riak_core:register(legolas, [{vnode_module, legolas_storage_vnode}]),

            ok = riak_core_ring_events:add_guarded_handler(legolas_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(legolas_node_event_handler, []),

            ok = riak_core_node_watcher:service_up(legolas, self()),
            ok = riak_core_node_watcher:service_up(legolas_storage, self()),

            {ok, Pid};

        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ?NOTICE("=== Legolas Stop ===", []),
    ok.

