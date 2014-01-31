%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2014-01-14 21:31:48
%%%------------------------------------------------------------ 

-module(legolas_sup).
-behaviour(supervisor).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([start_link/0]).

%% ------------------------------ Callbacks ------------------------------ 
-export([init/1]).

%% ============================== APIs ==============================
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ============================== Callbacks ==============================
%%

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

    EntropyManager = {legolas_entropy_manager,
                      {legolas_entropy_manager, start_link, []},
                      permanent, 30000, worker, [legolas_entropy_manager]},

    % Figure out which processes we should run...
    HasStorageBackend = (app_helper:get_env(legolas, storage_backend) /= undefined),

    %Riak_KV = {riak_kv_sup,
               %{riak_kv_sup, start_link, []},
               %permanent, infinity, supervisor, [riak_kv_sup]},

    Children = lists:flatten([
                VMaster,
                ?IF(HasStorageBackend, Storage, []),
                %Riak_KV,
                PutFSMs, 
                GetFSMs, 
                DeleteFSMs,
                EntropyManager
               ]),
    { ok, { {one_for_one, 5, 10}, Children}}.

