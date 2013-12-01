-module(legolas_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->

    legolas_entropy_info:create_table(),

    VMaster = { legolas_vnode_master,
                  {riak_core_vnode_master, start_link, [legolas_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    Storage = { legolas_storage_vnode_master,
                  {riak_core_vnode_master, start_link, [legolas_storage_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    PutFSMs = {legolas_put_data_fsm_sup,
                 {legolas_put_data_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [legolas_put_data_fsm_sup]},

    GetFSMs = {legolas_get_data_fsm_sup,
               {legolas_get_data_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [legolas_get_data_fsm_sup]},

    DeleteFSMs = {legolas_delete_data_fsm_sup,
               {legolas_delete_data_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [legolas_delete_data_fsm_sup]},

    Children = [
                VMaster, 
                Storage, 
                PutFSMs, 
                GetFSMs, 
                DeleteFSMs
               ],
    { ok, { {one_for_one, 5, 10}, Children}}.

