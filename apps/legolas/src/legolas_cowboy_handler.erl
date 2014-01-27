%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-06 13:32:02
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
         provide_resource/2,
         to_html/2
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
      {<<"application/octet-stream">>, provide_resource},
      {<<"text/html">>, to_html}
     ], Req, State}.

delete_resource(Req, State) ->
    {Url, _Req} = cowboy_req:path(Req),
    Path = binary_to_list(Url),
    case legolas:delete_data(Path) of
        ok -> 
            {true, Req, State};
        _ -> 
            {false, Req, State}
    end.

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
    ?DEBUG("Enter accept_resource/2", []),
    {Url, _Req} = cowboy_req:path(Req),
    Path = binary_to_list(Url),
    case cowboy_req:stream_body(infinity, Req) of
        {error, Reason} ->
            ?ERROR("cowboy_req:stream_body/2 fail. Reason: ~p", [Reason]),
            {false, Req, State};
        {done, Req2} ->
            {true, Req2, State};
        {ok, Data, Req2} ->
            legolas:put_data(Path, Data),
            {true, Req2, State}
    end.

provide_resource(Req, State) ->
    ?DEBUG("Enter provide_resource/2", []),
    {Url, _Req} = cowboy_req:path(Req),
    Path = binary_to_list(Url),
    ?DEBUG("Request path = ~p", [Path]),
    case legolas:get_data(Path) of
        {ok, Binary} ->
            {Binary, Req, State};
        {error, _Reason} ->
            {"", Req, State}
    end.


to_html(Req, State) ->
    ?DEBUG("Enter to_html/2", []),
    {Url, _Req} = cowboy_req:path(Req),
    Path = binary_to_list(Url),
    ?DEBUG("Request path = ~p", [Path]),
    {Method, _Req2} = cowboy_req:method(Req),
    ?DEBUG("Request Method = ~p", [Method]),
    case Method of
        "HEAD" ->
            Filename = legolas:path_to_filename(Path),
            ?DEBUG("HEAD filename: ~p", [Filename]),
            {"Filename: " ++ Filename, Req, State};
        _ -> {"", Req, State}
    end.

