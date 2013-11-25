%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-07 14:26:48
%%%------------------------------------------------------------ 

-module(legolas_put_data_fsm_sup).
-behaviour(supervisor).

-export([
         start_put_data_fsm/1,
         start_link/0
        ]).
-export([init/1]).

start_put_data_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    StorageFsm = {undefined,
                  {legolas_put_data_fsm, start_link, []},
                  temporary, 5000, worker, [legolas_put_data_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [StorageFsm]}}.

