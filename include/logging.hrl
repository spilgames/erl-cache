%%============================================================================
%% Logging convenience functions
%%============================================================================

-define(DEBUG(Msg, Args), _ = lager:log(debug, Msg, Args)).
-define(INFO(Msg, Args), _ = lager:log(info, Msg, Args)).
-define(NOTICE(Msg, Args), _ = lager:log(notice, Msg, Args)).
-define(WARNING(Msg, Args), _ = lager:log(warning, Msg, Args)).
-define(ERROR(Msg, Args), _ = lager:log(error, Msg, Args)).


