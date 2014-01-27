%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     用于异步遍历的工作者进程。
%%%
%%% @end
%%% Created : 2013-11-30 10:51:38
%%%------------------------------------------------------------ 

-module(legolas_worker).
-include("legolas.hrl").
-include("legolas_storage_vnode.hrl").
-behaviour(riak_core_vnode_worker).

%% ------------------------------ APIs ------------------------------ 
-export([
         init_worker/3,
         handle_work/3
        ]).

%% ------------------------------ record ------------------------------ 
-record(state, {
          partition :: partition()
         }).

%% ============================== APIs ==============================
%%

%% ------------------------------ init_worker ------------------------------ 
%% @doc Initialize the worker. Currently only the VNode index
%% parameter is used.
init_worker(Partition, _Args, _Props) ->
    {ok, #state{partition=Partition}}.

%% ------------------------------ handle_work ------------------------------ 
%% @doc Perform the asynchronous fold operation.
handle_work({fold, FoldFun, FinishFun}, _Sender, State) ->
    try
        FinishFun(FoldFun())
    catch
        receiver_down -> ok;
        stop_fold -> ok
    end,
    {noreply, State}.

