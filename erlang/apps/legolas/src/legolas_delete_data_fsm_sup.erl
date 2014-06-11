%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-07 23:27:28
%%%------------------------------------------------------------ 

-module(legolas_delete_data_fsm_sup).
-behaviour(supervisor).

-export([
         start_delete_data_fsm/1,
         start_link/0
        ]).
-export([init/1]).

start_delete_data_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DeleteFsm = {undefined,
                  {legolas_delete_data_fsm, start_link, []},
                  temporary, 5000, worker, [legolas_delete_data_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [DeleteFsm]}}.

