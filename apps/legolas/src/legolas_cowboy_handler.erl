%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 
%%%------------------------------------------------------------ 

-module(legolas_cowboy_handler).
-include("legolas.hrl").

%% Cowboy handler callbacks
-export([init/3,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         delete_completed/2,
         is_authorized/2,
         resource_exists/2,
         terminate/3
        ]).

-export([accept_resource/2,
         provide_resource/2
        ]).

-record(state, {test_data}).

%%%------------------------------------------------------------ 
%%% Cowboy handler callacks
%%%------------------------------------------------------------ 

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/octet-stream">>, accept_resource}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/octet-stream">>, provide_resource}
     ], Req, State}.

%% The value returned indicates if the action was successful, regardless of whether the resource is immediately deleted from the system.
delete_resource(Req, State) ->
    {false, Req, State}.

%% Return whether the delete action has been completed.
%% The function should return false if there is no guarantee that the resource gets deleted immediately from the system, including from any internal cache.
%% Whene this function returns false, a 202 Accepted response will be sent instead of a 200 OK or 204 Not Content.
delete_completed(Req, State) ->
    {false, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

is_authorized(Req, State) ->
    Authorized = true,
    %{ok, Auth, Req1} = cowboy_req:parse_header(<<"authorization">>, Req),
    %case Auth of
        %{<<"basic">>, {User = <<"username">>, <<"password">>}} ->
            %{true, Req1, User};
        %_ ->
            %{{false, <<"Basic realm=\"legolas\"">>}, Req1, State}
    %end.
    case Authorized of
        true ->
            {true, Req, #state{test_data = common_utils:read_file(legolas, "1m.dat")}};
            %{true, Req, State};
        false ->
            io:format("<<Legolas>> Not authorized!\n"),
            {false, Req, State}
    end.

resource_exists(Req, State) ->
    ResourceExists = true,
    {ResourceExists, Req, State}.

%%%------------------------------------------------------------ 
%%% Internal functions
%%%------------------------------------------------------------ 

accept_resource(Req, State) ->
    {true, Req, State}.

%provide_resource(Req, State) ->
    %Body = <<"{\"rest\": \"Hello World!\"}">>,
provide_resource(Req, #state{test_data = Body} = State) ->
    {Body, Req, State}.

