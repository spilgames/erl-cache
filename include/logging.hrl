%%============================================================================
%% Logging convenience functions
%%============================================================================

-define(DEBUG(Msg, Args), _ = lager:log(debug, self(), Msg, Args)).
-define(INFO(Msg, Args), _ = lager:log(info, self(), Msg, Args)).
-define(NOTICE(Msg, Args), _ = lager:log(notice, self(), Msg, Args)).
-define(WARNING(Msg, Args), _ = lager:log(warning, self(), Msg, Args)).
-define(ERROR(Msg, Args), _ = lager:log(error, self(), Msg, Args)).


