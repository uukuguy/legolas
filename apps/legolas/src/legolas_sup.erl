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

    %_UDBServer = { 
        %%% -------- ID --------
        %udbfs_server,                   
        %%% -------- Start -------- 
        %%%   {Module, Function, Arguments}
        %{udbfs_server, start_link, []}, 
        %%% -------- Restart -------- 
        %%% 子进程故障时是否重启
        %%%   permanent : 始终重启
        %%%   temporary : 永不重启
        %%%   transient : 仅在进程意外终止时重启
        %permanent,                      
        %%% -------- Shutdown -------- 
        %%% 如何终止进程
        %%%   整数 : 留给进程自我了断的时间（毫秒）
        %%%   brutal_kill : 立即中止子进程
        %%%   infinity : 子进程也是监督者进程，
        %%%              保证有充分时间退出
        %5000,                           
        %%% -------- Type -------- 
        %%% supervisor or worker 
        %worker,                         
        %%% -------- Dependences -------- 
        %%% 依赖模块, 用于代码热升级。
        %[udbfs_server]                  
    %},

    VNodeMaster = { 
        legolas_vnode_master,
        {riak_core_vnode_master, start_link, [legolas_vnode]},
        permanent, 5000, worker, [riak_core_vnode_master]},

    Storage = { 
        legolas_storage_vnode_master,
        {riak_core_vnode_master, start_link, [legolas_storage_vnode]},
        permanent, 5000, worker, [riak_core_vnode_master]},

    EntropyManager = {
        legolas_entropy_manager,
        {legolas_entropy_manager, start_link, []},
        permanent, 30000, worker, [legolas_entropy_manager]},


    PutFSMs = {legolas_put_data_fsm_sup,
                 {legolas_put_data_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [legolas_put_data_fsm_sup]},

    GetFSMs = {legolas_get_data_fsm_sup,
               {legolas_get_data_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [legolas_get_data_fsm_sup]},

    DeleteFSMs = {legolas_delete_data_fsm_sup,
               {legolas_delete_data_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [legolas_delete_data_fsm_sup]},

    % Figure out which processes we should run...
    LegolasStorageBackend = common_utils:get_env(legolas, storage_backend),
    ?DEBUG("legolas storage_backend: ~p", [LegolasStorageBackend]),
    HasStorageBackend = (LegolasStorageBackend /= undefined),

    %Riak_KV = {riak_kv_sup,
               %{riak_kv_sup, start_link, []},
               %permanent, infinity, supervisor, [riak_kv_sup]},

    Children = lists:flatten([
                VNodeMaster,
                ?IF(HasStorageBackend, Storage, []),
                %Riak_KV,
                PutFSMs, 
                GetFSMs, 
                DeleteFSMs,
                EntropyManager
               ]),

    %% 重启策略
    %% 生产系统通常每小时(3600秒)4次，调试阶段设为不重启0/1。
    RestartStrategy = {
        one_for_one, %% How 仅重启退出的子进程 
        0,           %% Max 在指定时间段内最大重启次数
        1            %% Within 指定时间段。
    },

    { ok, {RestartStrategy , Children}}.

