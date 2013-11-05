%-compile([{parse_transform, lager_transform}]).

%-define(PRINT(Var), io:format("[DEBUG] ~p:~p - (~p) : ~p~n", [?MODULE, ?LINE, ??Var, Var])).

%-define(LOG(Level, Color, Msg, Args), lager:log(Level, self(), string:concat(string:concat(string:concat(string:concat(" \e[1;34m<<-- legolas ", Color), " ~p.erl(Line ~p) - "), Msg), " \e[1;34m-->>"), lists:append([?MODULE, ?LINE], Args))).
%-define(LOG(Level, Color, Msg, Args), lager:log(Level, self(), string:concat(string:concat(string:concat(" \e[1;34m<<-- ~p.erl(\e[1;32mLine ~p\e[1;34m) - ", Color), Msg), " \e[1;34m-->>"), lists:append([?MODULE, ?LINE], Args))).
%-define(LOG(Level, Color, Msg, Args), lager:log(Level, self(), string:concat(string:concat(" \e[0;32m~p.erl(\e[0;34mLine ~p\e[0;32m) - ", Color), Msg), lists:append([?MODULE, ?LINE], Args))).
%-define(DEBUG(Msg, Args), lager:debug([{module, ?MODULE}], string:concat(string:concat(" \e[0;32m~p.erl(\e[0;34mLine ~p\e[0;32m) - ", "\e[0;38m"), Msg), lists:append([?MODULE, ?LINE], Args))).
%-define(DEBUG(Msg, Args), ?LOG(debug, "\e[0;38m", Msg, Args)).
%-define(INFO(Msg, Args), ?LOG(info, "\e[1;37m", Msg, Args)).
%-define(NOTICE(Msg, Args), ?LOG(notice, "\e[1;36m", Msg, Args)).
%-define(WARNING(Msg, Args), ?LOG(warning, "\e[1;33m", Msg, Args)).
%-define(ERROR(Msg, Args), ?LOG(error, "\e[1;31m", Msg, Args)).
%-define(CRITICAL(Msg, Args), ?LOG(critical, "\e[1;35m", Msg, Args)).

-define(DEBUG(Msg, Args), lager:debug([{module, ?MODULE}], string:concat(string:concat(" \e[0;32m<<-- legolas log -->> \e[0;38m", Msg), "\e[0;38m"), Args)).
-define(INFO(Msg, Args), lager:info([{module, ?MODULE}], string:concat(string:concat(" \e[0;32m<<-- legolas log -->> \e[1;37m", Msg), "\e[0;38m"), Args)).
-define(NOTICE(Msg, Args), lager:notice([{module, ?MODULE}], string:concat(string:concat(" \e[0;32m<<-- legolas log -->> \e[1;36m", Msg), "\e[0;38m"), Args)).
-define(WARNING(Msg, Args), lager:warning([{module, ?MODULE}], string:concat(string:concat(" \e[0;32m<<-- legolas log -->> \e[1;33m", Msg), "\e[0;38m"), Args)).
-define(ERROR(Msg, Args), lager:error([{module, ?MODULE}], string:concat(string:concat(" \e[0;32m<<-- legolas log -->> \e[1;31m", Msg), "\e[0;38m"), Args)).
-define(CRITICAL(Msg, Args), lager:critical([{module, ?MODULE}], string:concat(string:concat(" \e[0;32m<<-- legolas log -->> \e[1;35m", Msg), "\e[0;38m"), Args)).
