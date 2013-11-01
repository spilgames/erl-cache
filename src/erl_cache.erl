-module(erl_cache).

-export([start/0]).

%Examples
-include_lib("erl_cache.hrl").
-export([test/0, test/1]).

%% ====================================================================
%% API
%% ====================================================================

%% @doc Start the applications and all it's dependencies.
-spec start() -> {ok, fun(() -> ok)}.
%% @end
start() ->
    elibs_application:start(erl_cache).

%% ====================================================================
%% Examples
%% ====================================================================

?CACHE([{ttl, 5}, {evict, 5}]).
test() ->
    timer:sleep(1000),
    wake_up.

?CACHE([{ttl, 5}, {evict, 5}]).
test(Time) ->
    timer:sleep(Time),
    {wake_up, Time}.
