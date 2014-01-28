-ifndef(DECORATOR).
    -compile([{parse_transform, elibs_decorator}]).
    -define(DECORATOR, true).
-endif.

-include_lib("erlanglibs/include/transform.hrl").

%%====================================================================
%% ?CACHE([erl_cache:cache_set_option() | erl_cache:cache_get_option()]).
%%====================================================================

-define(CACHE(Options), -decorate({erl_cache_decorator, cache_pt, {?MODULE, ?FUNCTION, Options}})).
