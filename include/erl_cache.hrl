%%============================================================================
%% Application defaults
%%============================================================================

-define(DEFAULT_WAIT_FOR_REFRESH, true).
-define(DEFAULT_WAIT_UNTIL_CACHED, false).
-define(DEFAULT_VALIDITY, 300000).
-define(DEFAULT_EVICT, 60000).
-define(DEFAULT_REFRESH_CALLBACK, undefined).

%%============================================================================
%% Logging convenience functions
%%============================================================================

-define(DEBUG(Msg, Args), _ = lager:log(debug, Msg, Args)).
-define(INFO(Msg, Args), _ = lager:log(info, Msg, Args)).
-define(NOTICE(Msg, Args), _ = lager:log(notice, Msg, Args)).
-define(WARNING(Msg, Args), _ = lager:log(warning, Msg, Args)).
-define(ERROR(Msg, Args), _ = lager:log(error, Msg, Args)).

%%============================================================================
%% Parse transform for the ?CACHE decorator
%%============================================================================

-compile([{parse_transform, decorator_pt_core}]).

%% ?CACHE(erl_cache:name(), [erl_cache:cache_set_option() | erl_cache:cache_get_option()]).
-define(CACHE(Name, Options), -decorate({erl_cache_decorator, cache_pt, {?MODULE, ?FUNCTION, Name, Options}})).

