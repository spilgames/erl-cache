-module(erl_cache_eunit).

-include_lib("eunit/include/eunit.hrl").
-include("erl_cache.hrl").
-include_lib("decorator_pt/include/decorator_pt.hrl").

-ifdef(TEST).
-export([wait/1]).
-endif.

%TODO turn this app into an instantiator of cache_servers
%TODO Documentation
%TODO dialyzer

setup() ->
    ok = application:start(erl_cache),
    Old = application:get_env(erl_cache, wait_for_refresh),
    ok = application:set_env(erl_cache, wait_for_refresh, false),
    [fun () -> application:stop(erl_cache) end,
     fun () -> case Old of
                    undefined -> application:unset_env(erl_cache, wait_for_refresh);
                    {ok, Val} -> application:set_env(erl_cache, wait_for_refresh, Val)
                end
        end
    ].

cleanup(Funs) ->
    lists:foreach(fun (F) -> F() end, Funs).

get_set_evict_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
            {"Catch faulty options in get and set", fun wrong_opts_get_set/0},
            {"Default get, set, evict and stats", fun default_get_set_evict/0},
            {"Get and set with undefined refresh callback", fun refresh_undefined/0},
            {"Get and set with refresh stale_async_mfa", fun refresh_stale_async_mfa/0},
            {"Get and set with refresh stale_sync_closure", fun refresh_stale_sync_closure/0},
            {"Stats after basic operations", fun stats/0},
            {"Parse transform basic usage", fun parse_transform/0}
    ]}.

%% Test Sets

wrong_opts_get_set() ->
    ?assertEqual({error, {invalid, validity}}, erl_cache:set(key, value, [{validity, -1}])),
    ?assertEqual({error, {invalid, evict}}, erl_cache:set(key, value, [{validity, 1}, {evict, 0}])),
    ?assertEqual({error, {invalid, wait_for_refresh}},
                 erl_cache:get(key, [{wait_for_refresh, 1}])),
    ?assertEqual({error, {invalid, wait_until_cached}},
                 erl_cache:set(key, value, [{wait_until_cached, "true"}])),
    ?assertEqual({error, {invalid, refresh_callback}},
                 erl_cache:set(key, value, [{refresh_callback, myfun}])),
    ?assertEqual({error, {invalid, refresh_callback}},
                 erl_cache:set(key, value, [{refresh_callback, {mymodule, myfun}}])).

default_get_set_evict() ->
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 1)),
    ?assertEqual(ok, set_in_cache(test_key, <<"test_value">>, [], 1)),
    ?assertEqual({ok, <<"test_value">>}, get_from_cache(test_key, [], 1)),
    ?assertEqual({ok, <<"test_value">>}, erl_cache:get(test_key)),
    ?assertEqual(ok, erl_cache:set(test_key2, foo)),
    timer:sleep(1),
    ?assertEqual({ok, foo}, erl_cache:get(test_key2)),
    ?assertEqual(ok, evict_from_cache(test_key)),
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 1)).

refresh_undefined() ->
    SetOpts = [{refresh_callback, undefined}, {validity, 50}, {evict, 300}],
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 1)),
    TestValue = os:timestamp(),
    ?assertEqual(ok, set_in_cache(test_key, TestValue, SetOpts, 1)),
    ?assertEqual({ok, TestValue}, get_from_cache(test_key, [], 1)),
    % This get should hit the value in stale and shouldn't refresh it
    ?assertEqual({ok, TestValue}, get_from_cache(test_key,
                                                 [{refresh_function, {os, timestamp}}], 50)),
    % At this point the value should have been evicted
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 300)).

refresh_stale_async_mfa() ->
    SetOpts = [{refresh_callback, {os, timestamp, []}}, {validity, 50}, {evict, 300}],
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 1)),
    TestValue = os:timestamp(),
    ?assertEqual(ok, set_in_cache(test_key, TestValue, SetOpts, 1)),
    ?assertEqual({ok, TestValue}, get_from_cache(test_key, [], 1)),
    % This get should hit the value in stale and refresh it
    ?assertMatch({ok, TestValue}, get_from_cache(test_key, [{wait_for_refresh, false}], 50)),
    % The value should have been asynchronously refreshed
    ?assertMatch({ok, T} when is_tuple(T) andalso T/=TestValue,
                 get_from_cache(test_key, [{wait_for_refresh, false}], 10)),
    % At this point the value should have been evicted
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 400)).

refresh_stale_sync_closure() ->
    SetOpts = [{refresh_callback, fun() -> os:timestamp() end},
               {validity, 50}, {evict, 300}],
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 1)),
    TestValue = os:timestamp(),
    ?assertEqual(ok, set_in_cache(test_key, TestValue, SetOpts, 1)),
    ?assertEqual({ok, TestValue}, get_from_cache(test_key, [], 1)),
    % This get should hit the value in stale and wait for the refresh
    {ok, UpdatedValue} = get_from_cache(test_key, [{wait_for_refresh, true}], 50),
    ?assertMatch(T when is_tuple(T) andalso T/=TestValue, UpdatedValue),
    % Since it has just been refreshed, the updated value should be there
    ?assertMatch({ok, UpdatedValue},
                 get_from_cache(test_key, [{wait_for_refresh, false}], 10)),
    % At this point the value should have been evicted
    ?assertEqual({error, not_found}, get_from_cache(test_key, [], 350)).

stats() ->
    erl_cache:get(foo),
    erl_cache:set(foo, bar, [{validity, 30}, {evict, 100}]),
    erl_cache:set(foo2, bar, [{validity, 1}, {evict, 1}]),
    erl_cache:set(foo3, bar, [{validity, 1}, {evict, 1}]),
    get_from_cache(foo, [], 1),
    get_from_cache(foo, [], 1),
    get_from_cache(foo, [], 1),
    get_from_cache(foo, [], 50),
    get_from_cache(foo, [], 1),
    erl_cache:evict(foo),
    erl_cache:get(foo2),
    timer:sleep(100),
    Stats = erl_cache_server:get_stats(),
    ?assertEqual(3, proplists:get_value(set, Stats)),
    ?assertEqual(11, proplists:get_value(total_ops, Stats)),
    ?assertEqual(3, proplists:get_value(hit, Stats)),
    ?assertEqual(2, proplists:get_value(stale, Stats)),
    ?assertEqual(1, proplists:get_value(miss, Stats)),
    % foo is manually evicted, foo2 is evicted after the miss, foo3 is never evicted
    ?assertEqual(2, proplists:get_value(evict, Stats)),
    ?assertEqual(1, proplists:get_value(entries, Stats)),
    ?assertMatch(N when is_integer(N) andalso N>0, proplists:get_value(memory, Stats)).

parse_transform() ->
    {Time1, ok} = timer:tc(?MODULE, wait, [1000]),
    {Time2, ok} = timer:tc(?MODULE, wait, [1000]),
    ?assertMatch(N when N>1000000, Time1),
    ?assertMatch(N when N<1000000, Time2).

%% Internal functions

set_in_cache(K, V, Opts, SleepAfter) ->
    apply(erl_cache, set, [K, V, Opts]),
    timer:sleep(SleepAfter).

get_from_cache(K, Opts, SleepBefore) ->
    timer:sleep(SleepBefore),
    apply(erl_cache, get, [K, Opts]).

evict_from_cache(K) ->
    erl_cache:evict(K).

?CACHE([{validity, 1000}, {evict, 100}, {wait_until_cached, true}]).
wait(Time) ->
    timer:sleep(Time),
    ok.
