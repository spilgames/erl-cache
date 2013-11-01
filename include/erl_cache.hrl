-ifndef(DECORATOR).
    -compile([{parse_transform, elibs_decorator}]).
    -define(DECORATOR, true).
-endif.

-include_lib("erlanglibs/include/transform.hrl").

%%====================================================================
%% Spec:
%%      ?CACHE(Options::erl_cache_facade:erl_cache_options()).
%% Example:
%%      ?AUTHENTICATE([{ttl, default}, {evict_after, default}, {refresh, never}]).
%% Parameters:
%%
%% Options
%%      see      : erl_cache_facade:erl_cache_options() documentation
%%====================================================================

-ifdef(TEST).
    -ifdef(ENABLE_CACHE_DECORATOR).
        -define(CACHE(Options), -decorate({erl_cache_decorator, cache_pt, {?MODULE, ?FUNCTION, Options}})).
    -else.
        -define(CACHE(Options), -decorate({})).
    -endif.
-else.
    -define(CACHE(Options), -decorate({erl_cache_decorator, cache_pt, {?MODULE, ?FUNCTION, Options}})).
-endif.
