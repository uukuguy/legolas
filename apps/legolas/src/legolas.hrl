%-compile([{parse_transform, lager_transform}]).

-define(PRINT(Var), io:format("[DEBUG] ~p:~p - (~p) : ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(DEBUG(Var), io:format("~p [debug] ~p:~p - ~p~n", [time(), ?MODULE, ?LINE, Var])).
%-define(DEBUG(Msg, Args), lager:debug(<<"[debug] ", ?MODULE, ":", ?LINE, " - ", Msg>>, Args)).
-define(INFO(Msg, Args), lager:info(Msg, Args)).
-define(WARNING(Msg, Args), lager:warning(Msg, Args)).
-define(ERROR(Msg, Args), lager:ERROR(Msg, Args)).
-define(CRITICAL(Msg, Args), lager:critical(Msg, Args)).
