%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 
%%%------------------------------------------------------------ 

-module(common_utils).

-export([
         get_env/3,
         read_file/2,
         full_path/2,
         file_exists/2,
         valid_path/1,
         escape_html_chars/1
        ]).

%%%------------------------------------------------------------ 
%%% environment
%%%------------------------------------------------------------ 
get_env(App, Par, Default) ->
    case application:get_env(App, Par) of
        {ok, Value} -> Value;
         _ -> Default
    end.

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
read_file(App, Name) ->
    {ok, Binary} = file:read_file(full_path(App, Name)),
    Binary.

full_path(App, Name) ->
    filename:join([code:priv_dir(App), Name]).

file_exists(App, Name) ->
    case file:read_file_info(full_path(App, Name)) of
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


