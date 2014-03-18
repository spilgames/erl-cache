%%============================================================================
%% Application defaults
%%============================================================================

-define(DEFAULT_WAIT_FOR_REFRESH, true).
-define(DEFAULT_WAIT_UNTIL_DONE, false).
-define(DEFAULT_VALIDITY, 300000).
-define(DEFAULT_ERROR_VALIDITY, 30000).
-define(DEFAULT_EVICT, 60000).
-define(DEFAULT_REFRESH_CALLBACK, undefined).
-define(DEFAULT_MAX_CACHE_SIZE, undefined).

-define(CACHE_SIZE_CHECK_INTERVAL, 10000).
-define(DEFAULT_MEM_CHECK_INTERVAL, 10000).

%%============================================================================
%% Parse transform for the ?CACHE decorator
%%============================================================================

-compile([{parse_transform, decorator_pt_core}]).

%% ?CACHE(erl_cache:name(), [erl_cache:cache_set_option() | erl_cache:cache_get_option()]).
-define(CACHE(Name, Options), -decorate({erl_cache_decorator, cache_pt, {?MODULE, ?FUNCTION, Name, Options}})).

