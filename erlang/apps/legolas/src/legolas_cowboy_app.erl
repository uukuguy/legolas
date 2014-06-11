%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-04 15:33:10
%%%------------------------------------------------------------ 

-module(legolas_cowboy_app).
-behaviour(application).

%% API
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, _Pid} = legolas_cowboy_sup:start_link(),
    ok.

stop(_State) ->
    ok.
