%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2014-01-15 03:34:43
%%%------------------------------------------------------------ 
-module(legolas_keys_fsm_sup).
-behaviour(supervisor).

-export([start_keys_fsm/2]).
-export([start_link/0]).
-export([init/1]).

start_keys_fsm(Node, Args) ->
    case supervisor:start_child({?MODULE, Node}, Args) of 
        {ok, Pid} ->
            riak_kv_stat:update({list_create, Pid}),
            {ok, Pid};
        Error ->
            riak_kv_stat:update(list_create_error),
            Error
    end.

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    KeysFsmSpec = {undefined,
               {riak_core_coverage_fsm, start_link, [legolas_keys_fsm]},
               temporary, 5000, worker, [legolas_keys_fsm]},

    {ok, {{simple_one_for_one, 10, 10}, [KeysFsmSpec]}}.
