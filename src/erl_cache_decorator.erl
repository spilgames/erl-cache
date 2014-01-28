-module(erl_cache_decorator).

-export([cache_pt/3]).

%% ====================================================================
%% API
%% ====================================================================

-spec cache_pt(function(), [term()], {atom(), atom(),
               erl_cache:cache_options()}) -> fun(() -> term()).
cache_pt(Fun, Args, {Module, FunctionAtom, Options}) ->
    FilteredOpts = lists:map(fun(K) -> {K, proplists:get_value(K, Options)} end,
                             [validity, evict, wait_until_cached, wait_for_refresh])
                   ++ [{refresh_callback, {Module, FunctionAtom, Args}}],
    Key = {decorated, Module, FunctionAtom, erlang:phash2(Args)},
    FromCache = erl_cache:get(Key, FilteredOpts),
    case FromCache of
        {ok, Result} -> fun() -> Result end;
        {error, not_found} ->
            fun () ->
                    Res = Fun(Args),
                    ok = erl_cache:set(Key, Res, FilteredOpts),
                    Res
            end;
        {error, Err} ->
            throw({error, {cache_pt, Err}})
    end.

