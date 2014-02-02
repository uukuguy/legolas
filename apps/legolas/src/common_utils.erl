%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 
%%%------------------------------------------------------------ 

-module(common_utils).
-include("global.hrl").

-export([
         enable_console_debug/2,
         get_env/1,
         get_env/2,
         get_env/3,
         get_prop_or_env/3,
         get_prop_or_env/4,
         get_config_value/3,
         get_nested_config/3,
         binary_to_string/1,
         string_to_binary/1,
         make_vtag/1,
         integer_to_list/2,
         integer_to_fixed_list/3,
         different/1,
         new_id/1,
         random_id/0,
         read_file/1,
         write_file/2,
         delete_file/1,
         ensure_path/1,
         file_exists/1,
         valid_path/1,
         escape_html_chars/1
        ]).

-compile({no_auto_import, [integer_to_list/2]}).


-spec enable_console_debug(boolean(), [module()]) -> ok.
enable_console_debug(Enabled, Modules) ->
    case Enabled of
        true ->
            [lager:trace_console([{module, Mod}]) || Mod <- Modules];
        false ->
            [begin
                 {ok, Trace} = lager:trace_console([{module, Mod}]),
                 lager:stop_trace(Trace)
             end || Mod <- Modules]
    end,
    ok.

%%%------------------------------------------------------------ 
%%% environment
%%%------------------------------------------------------------ 

%% @spec get_env(App :: atom()) -> [{Key :: atom(), Value :: term()}]
get_env(App) ->
    application:get_all_env(App).

%% @spec get_env(App :: atom(), Key :: atom()) -> term()
get_env(App, Key) ->
    get_env(App, Key, undefined).

%% @spec get_env(App :: atom(), Key :: atom(), Default :: term()) -> term()
get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
	{ok, Value} ->
            Value;
        _ ->
            Default
    end.

%% @doc Retrieve value for Key from Properties if it exists, otherwise
%%      return from the application's env.
-spec get_prop_or_env(atom(), [{atom(), term()}], atom()) -> term().
get_prop_or_env(Key, Properties, App) ->
    get_prop_or_env(Key, Properties, App, undefined).

%% @doc Return the value for Key in Properties if it exists, otherwise return
%%      the value from the application's env, or Default.
-spec get_prop_or_env(atom(), [{atom(), term()}], atom(), term()) -> term().
get_prop_or_env(Key, Properties, App, Default) ->
    case proplists:get_value(Key, Properties) of
        undefined ->
            get_env(App, Key, Default);
        Value ->
            Value
    end.

%%%------------------------------------------------------------ 
%%% Configure
%%%------------------------------------------------------------ 

get_config_value(Key, Config, Default) ->
    case orddict:find(Key, Config) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.

get_nested_config(Key, Config, Category) ->
    case proplists:get_value(Key, Config) of
        undefined ->
            case proplists:get_value(Category, Config) of
                undefined ->
                    undefined;
                {InnerConfig, SubCategory} ->
                    get_nested_config(Key, InnerConfig, SubCategory);
                InnerConfig ->
                    proplists:get_value(Key, InnerConfig)
            end;
        Val ->
            Val
    end.

-spec make_vtag(erlang:timestamp()) -> list().
make_vtag(Now) ->
    <<HashAsNum:128/integer>> = crypto:hash(md5, (term_to_binary({node(), Now}))),
    integer_to_list(HashAsNum,62).

%% @spec integer_to_list(Integer :: integer(), Base :: integer()) ->
%%          string()
%% @doc Convert an integer to its string representation in the given
%%      base.  Bases 2-62 are supported.

calc_power(X, Base, P) ->
    X1 = X / Base,
    case X1 > Base of
        true -> calc_power(X1, Base, P + 1);
        false -> P + 1
    end.

append_zero(Str, Remains) ->
    case Remains >= 1 of
        true -> append_zero("0" ++ Str, Remains - 1);
        false -> Str
    end.

integer_to_fixed_list(I, Base, Len) ->
    Str = integer_to_list(I, Base),
    P = calc_power(math:pow(2, Len), Base, 0) + 1,
    Remains = P - length(Str),
    %?NOTICE("fixed Str = ~p Remains = ~p", [Str, Remains]),
    case Remains > 0 of
        true -> append_zero(Str, Remains);
        false -> Str
    end.

integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base)
  when is_integer(I), is_integer(Base),Base >= 2, Base =< 1+$Z-$A+10+1+$z-$a ->
    if I < 0 ->
            [$-|integer_to_list(-I, Base, [])];
       true ->
            integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

%% @spec integer_to_list(integer(), integer(), string()) -> string()
integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 36 ->
		 [D-36+$a|R0];
	    D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.

%%%------------------------------------------------------------ 
%%% binary convert
%%%------------------------------------------------------------ 
binary_to_string(Bin) ->
    binary_to_list(to_hex(Bin)).

string_to_binary(Str) ->
   list_to_binary(from_hex(Str)). 

%% @spec to_hex(Bin::binary()) -> binary()
to_hex(Bin) ->
    << << (nib2hex(N)):8 >> || <<N:4>> <= Bin >>.

%% @spec from_hex(Bin::binary()) -> binary()
from_hex(Bin) ->
    << << (hex2nib(H)):4 >> || <<H:8>> <= Bin >>.

nib2hex(N) when 0 =< N, N =< 9 -> $0 + N;
nib2hex(N) when 10 =< N, N =< 15 -> $A + 10.

hex2nib(C) when $0 =< C, C =< $9 -> C - $0;
hex2nib(C) when $A =< C, C =< $F -> C - $A + 10.

%%%------------------------------------------------------------ 
%%% different
%%%------------------------------------------------------------ 
different(A) -> 
    fun(B) -> 
            A =/= B 
    end.

%%%------------------------------------------------------------ 
%%% random_id
%%%------------------------------------------------------------ 
random_id() ->
    erlang:phash2({self(), os:timestamp()}). % only has to be unique per-pid
    %erlang:phash2(erlang:now()).

%%%------------------------------------------------------------ 
%%% new id
%%% 生成Len个字符（[a-zA-Z0-1]）长度的随机字符串.
%%%------------------------------------------------------------ 
new_id(Len) ->
    Initial = random:uniform(62) - 1,
    new_id(<<Initial>>, Len).
new_id(Bin, 0) ->
    Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
    << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
new_id(Bin, Rem) ->
    Next = random:uniform(62) - 1,
    new_id(<<Bin/binary, Next>>, Rem - 1).

%%%------------------------------------------------------------ 
%%% file & path
%%%------------------------------------------------------------ 
-spec ensure_path(string()) -> {ok, exists} | {ok, created} | {error, term()}.
ensure_path(Path) ->
    case file:read_file_info(Path) of
        {ok, _Info} -> {ok, exists};
        {error, Reason} -> 
            ?NOTICE("Dir does not exist, Create it! Reason: ~p Path: ~p", [Reason, Path]),
            PathList = string:tokens(Path, "/"),
            case recursively_ensure_path(PathList, "") of
                ok -> {ok, created};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec recursively_ensure_path([nonempty_string()], string()) -> ok | {error, term()}.
recursively_ensure_path([], _Dir) -> ok;
recursively_ensure_path([Path | PathList], Dir) ->
    DirList = filename:join(Dir, Path),
    case filelib:is_dir(DirList) of
        true ->
            recursively_ensure_path(PathList, DirList);
        false ->
            case file:make_dir(DirList) of
                ok ->
                    recursively_ensure_path(PathList, DirList);
                {error, Reason} -> {error, Reason}
            end
    end.

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> {ok, Binary};
        {error, enoent} ->
            Reason = "File not exist.",
            ?ERROR("read_file ~p fail. Reason: ~p", [Filename, Reason]),
            {error, Reason};
        {error, Reason} -> 
            ?ERROR("read_file ~p fail. Reason: ~p", [Filename, Reason]),
            {error, Reason}
    end.

write_file(Filename, Data) ->
    case file:write_file(Filename, Data) of
        ok -> ok;
        {error, Reason} -> 
            ?ERROR("write_file ~p fail. Reason: ~p", [Filename, Reason]),
            {error, Reason}
    end.

delete_file(Filename) ->
    case file:delete(Filename) of
        ok -> ok;
        {error, enoent} ->
            Reason = "File not exist.",
            ?ERROR("delete_file ~p fail. Reason: ~p", [Filename, Reason]),
            {error, Reason};
        {error, Reason} -> 
            ?ERROR("delete_file ~p fail. Reason: ~p", [Filename, Reason]),
            {error, Reason}
    end.

file_exists(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _Info} -> true;
        {error, _Reason} -> false
    end.

valid_path(<<>>) ->true;
valid_path(<<$., _T/binary>>) -> false;
valid_path(<<$/, _T/binary>>) -> false;
valid_path(<<_Char, T/binary>>) -> valid_path(T).

%%%------------------------------------------------------------ 
%%% 
%%%------------------------------------------------------------ 
escape_html_chars(Bin) ->
    << <<(escape_html_char(B))/binary>> || <<B>> <= Bin >>.

escape_html_char($<) -> <<"&lt;">>;
escape_html_char($>) -> <<"&gt;">>;
escape_html_char($&) -> <<"&amp;">>;
escape_html_char(C) -> <<C>>.


