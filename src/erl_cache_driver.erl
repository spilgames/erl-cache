-module(erl_cache_driver).

-export([new/1, run/4]).

-ignore_xref([
    {basho_bench_config, get, 2}
]).

new(1) ->
    ok = erl_cache:start(),
    ok = erl_cache:start_cache(s1, basho_bench_config:get(cache_opts, [])),
    {ok, []};

new(_) ->
    {ok, []}.

run(get, KeyGen, _ValueGen, State) ->
    erl_cache:get(s1, KeyGen()),
    {ok, State};

run(set, KeyGen, ValueGen, State) ->
    erl_cache:set(s1, KeyGen(), ValueGen()),
    {ok, State}.

