%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2013-11-06 13:32:02
%%%------------------------------------------------------------ 

-module(legolas_cowboy_handler).
-include("global.hrl").

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

%% ------------------------------ delete_resource ------------------------------ 
delete_resource(Req, State) ->
    {Url, _Req} = cowboy_req:path(Req),
    Func = delete_resource_func(Req, State),
    decode_url(delete_resource, Url, Func).

delete_resource_func(Req, State) ->
    fun(Path, Filesystem) ->
            DeleteDataFunc = delete_resource_deletedata_func(),
            ?DEBUG("delete_resource_func DeleteDataFunc: ~p Path: ~p", [DeleteDataFunc, Path]),
            case DeleteDataFunc(Path, Filesystem) of
                ok -> 
                    {true, Req, State};
                _ -> 
                    {false, Req, State}
            end
    end.

delete_resource_deletedata_func() ->
    fun(Path, Filesystem) ->
            case Filesystem of
                udbfs -> udbfs:delete_data(Path);
                legolas -> legolas:delete_data(Path)
            end
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

%% ------------------------------ decode_url ------------------------------ 
%% @doc 根据url模式判断使用何种底层文件系统（包括udbfs和legolas两种）
%%      http://<ip>:<port>/udbfs/.... 指定使用udbfs文件系统。
%%
decode_url(Op, Url, Func) ->
    case Url of
        <<"/udbfs", Path/bitstring>> ->
            ?DEBUG("Decode <udbfs> ~p Url = ~p", [Op, Url]),
           Func(Path, udbfs); 
        _ ->
            ?DEBUG("Decode <legolas> ~p Url = ~p", [Op, Url]),
            Path = Url,
            Func(Path, legolas)
    end.

%% ------------------------------ accept_resource ------------------------------ 
accept_resource(Req, State) ->
    {Url, _Req} = cowboy_req:path(Req),
    Func = accept_resource_func(Req, State),
    decode_url(accept_resource, Url, Func).

accept_resource_func(Req, State) ->
    fun(Path, Filesystem) ->
            PutDataFunc = accept_resource_putdata_func(),
            ?DEBUG("accept_resource_func GetDataFunc: ~p Path: ~p", [PutDataFunc, Path]),
            case cowboy_req:stream_body(infinity, Req) of
                {error, Reason} ->
                    ?ERROR("cowboy_req:stream_body/2 fail. Reason: ~p", [Reason]),
                    {false, Req, State};
                {done, Req2} ->
                    {true, Req2, State};
                {ok, Data, Req2} ->
                    PutDataFunc(Path, Data, Filesystem),
                    {true, Req2, State}
            end
    end.

accept_resource_putdata_func() ->
    fun(Path, Data, Filesystem) ->
            case Filesystem of
                udbfs -> udbfs:put_data(Path, Data);
                legolas -> legolas:put_data(Path, Data)
            end
    end.

%% ------------------------------ provide_resource ------------------------------ 
provide_resource(Req, State) ->
    {Url, _Req} = cowboy_req:path(Req),
    Func = provide_resource_func(Req, State),
    decode_url(provide_resource, Url, Func).

provide_resource_func(Req, State) ->
    fun(Path, Filesystem) ->
            GetDataFunc = provide_resource_getdata_func(),
            ?DEBUG("provide_resource_func GetDataFunc: ~p Path: ~p", [GetDataFunc, Path]),
            case GetDataFunc(Path, Filesystem) of
                {ok, Binary} ->
                    {Binary, Req, State};
                {error, _Reason} ->
                    {"", Req, State}
            end
    end.

provide_resource_getdata_func() ->
    fun(Path, Filesystem) ->
            case Filesystem of
                udbfs -> udbfs:get_data(Path);
                legolas -> legolas:get_data(Path)
            end
    end.

%% ------------------------------ to_html ------------------------------ 
to_html(Req, State) ->
    ?DEBUG("Enter to_html/2", []),
    {Url, _Req} = cowboy_req:path(Req),
    Path = Url, %% binary_to_list(Url),
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

