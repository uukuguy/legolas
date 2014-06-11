%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     legolas存储后端接口。
%%%
%%% @end
%%% Created : 2013-11-18 16:32:50
%%%------------------------------------------------------------ 
-module(legolas_backend).

-export([
         behaviour_info/1,
         callback_after/3
        ]).

-type fold_buckets_fun() :: fun((binary(), any()) -> any() | no_return()).
-type fold_keys_fun() :: fun((binary(), binary(), any()) -> any() |
                                                            no_return()).
-type fold_objects_fun() :: fun((binary(), binary(), term(), any()) ->
                                       any() |
                                       no_return()).
-export_type([fold_buckets_fun/0,
              fold_keys_fun/0,
              fold_objects_fun/0]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [
     {api_version,0},
     {capabilities, 1},  % (State)
     {capabilities, 2},  % (Bucket, State)
     {start,2},          % (Partition, Config)
     {stop,1},           % (State)
     {get,3},            % (Bucket, Key, State)
     {put,5},            % (Bucket, Key, IndexSpecs, Val, State)
     {delete,4},         % (Bucket, Key, IndexSpecs, State)
     {drop,1},           % (State)
     {fold_buckets,4},   % (FoldBucketsFun, Acc, Opts, State),
                         %   FoldBucketsFun(Bucket, Acc)
     {fold_keys,4},      % (FoldKeysFun, Acc, Opts, State),
                         %   FoldKeysFun(Bucket, Key, Acc)
     {fold_objects,4},   % (FoldObjectsFun, Acc, Opts, State),
                         %   FoldObjectsFun(Bucket, Key, Object, Acc)
     {is_empty,1},       % (State)
     {status,1},         % (State)
     {callback,3}];      % (Ref, Msg, State) ->
behaviour_info(_Other) ->
    undefined.

%% Queue a callback for the backend after Time ms.
-spec callback_after(integer(), reference(), term()) -> reference().
callback_after(Time, Ref, Msg) when is_integer(Time), is_reference(Ref) ->
    riak_core_vnode:send_command_after(Time, {backend_callback, Ref, Msg}).

