%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 
%%%------------------------------------------------------------ 
-module(legolas_cowboy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_HTTP_PORT, 19090).
-define(DEFAULT_HTTP_LISTENERS, 10).

%%%------------------------------------------------------------ 
%%% API functions
%%%------------------------------------------------------------ 

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%------------------------------------------------------------ 
%%% Supervisor callacks
%%%------------------------------------------------------------ 

init([]) ->

    ListenPort = common_utils:get_env(legolas, http_port, ?DEFAULT_HTTP_PORT),
    Listeners = common_utils:get_env(legolas, http_listeners, ?DEFAULT_HTTP_LISTENERS),
    Dispatch = cowboy_router:compile(
                 [
                  %% {HostMatch, list({PathMatch, Handler, Opts})}
                  {'_', [{'_', legolas_cowboy_handler, []}]}
                 ]),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    %{ok, _} = cowboy:start_http(legolas_cowboy, Listeners,
                                %[{port, ListenPort}],
                                %[{env, [{dispatch, Dispatch}]}]
                               %),
    %ChildSpecs = [],

    ChildSpecs = [ranch:child_spec(legolas_cowboy, Listeners,
                                   ranch_tcp, 
                                   [
                                    {port, ListenPort}
                                   ], cowboy_protocol, 
                                   [{env, [{dispatch, Dispatch}]}]
                                  )],

    io:format("<<Legolas>> Cowboy starts on port ~p\n", [ListenPort]),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, ChildSpecs}}.

%%%------------------------------------------------------------ 
%%% Internal functions
%%%------------------------------------------------------------ 

                 

