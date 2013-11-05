%-compile([{parse_transform, lager_transform}]).

-define(PRINT(Var), io:format("[DEBUG] ~p:~p - (~p) : ~p~n", [?MODULE, ?LINE, ??Var, Var])).

-define(LOG(Level, Msg, Args), lager:log(Level, self(), string:concat(" ~p.erl(Line ~p) - ", Msg), lists:append([?MODULE, ?LINE], Args))).
-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(INFO(Msg, Args), ?LOG(info, Msg, Args)).
-define(WARNING(Msg, Args), ?LOG(warning, Msg, Args)).
-define(ERROR(Msg, Args), ?LOG(error, Msg, Args)).
-define(CRITICAL(Msg, Args), ?LOG(critical, Msg, Args)).
