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

%-record(state, {test_data}).

%%%------------------------------------------------------------ 
%%% Cowboy handler callacks
%%%------------------------------------------------------------ 

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/octet-stream">>, accept_resource}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/octet-stream">>, provide_resource}
     ], Req, State}.

delete_resource(Req, State) ->
    {false, Req, State}.

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
    {Authorized, Req, State}.

resource_exists(Req, State) ->
    ResourceExists = true,
    {ResourceExists, Req, State}.

%%%------------------------------------------------------------ 
%%% Internal functions
%%%------------------------------------------------------------ 

accept_resource(Req, State) ->
    NewID = common_utils:new_id(16),
    ?DEBUG("NewID = ~p", [NewID]),
    %{ok, [{<<"paste">>, Paste}], Req2} = cowboy_req:body_qs(Req),
    %{ok, [Paste}], Req2} = cowboy_req:body_qs(Req),
    {ok, [{Data, true}], Req2} = cowboy_req:body_qs(infinity, Req),
    ?DEBUG("After cowboy_req:body_qs", []),
    legolas:store_data(NewID, Data),
    ?DEBUG("After legolas:store_data", []),
    case cowboy_req:method(Req2) of
        {<<"POST">>, Req3} ->
            {{true, <<$/, NewID/binary>>}, Req3, State};
        {_, Req3} ->
            {true, Req3, State}
    end.

provide_resource(Req, State) ->
    case common_utils:read_file(legolas, "data/1m.dat") of
        {ok, Body} -> {Body, Req, State};
        error -> {"", Req, State}
    end.

