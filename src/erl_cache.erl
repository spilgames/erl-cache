-module(erl_cache).

-include_lib("eunit/include/eunit.hrl").

-export([
        get/1, get/2,
        set/2, set/3,
        evict/1
    ]).

-type refresh_options() :: never | synchronous | asynchronous.
-type refresh_function() :: function() | {atom(), atom()}.
-type cache_option() ::
    %% Mark entry as stale after X ms
    {ttl, default | pos_integer()} |
    %% Evict stale entries after X ms
    {evict, default | non_neg_integer()} |
    %% Refresh closure
    {refresh_function, undefined | refresh_function()} |
    %% Refresh closure args
    {refresh_args, undefined | [term()]} |
    %% Refresh miss, with this strategy
    {refresh_miss, default | refresh_options()} |
    %% Refresh stale, non-evicted entries with this strategy
    {refresh_stale, default | refresh_options()} |
    %% Refresh evicted, non-deleted entries with this strategy
    {refresh_evict, default | refresh_options()}.
-type cache_options() :: [cache_option()].
-type key() :: term().
-type value() :: term().

-export_type([
        refresh_options/0,
        cache_option/0, cache_options/0,
        key/0, value/0
    ]).

-define(DEFAULT_TTL, 300000).
-define(DEFAULT_EVICT, 60000).
-define(DEFAULT_REFRESH_MISS, never).
-define(DEFAULT_REFRESH_STALE, never).
-define(DEFAULT_REFRESH_EVICT, never).

%% ====================================================================
%% API
%% ====================================================================

-spec get(key()) ->
    {error, not_found} |
    {ok, value()}.
get(Key) ->
    get(Key, []).

-spec get(key(), cache_options()) ->
    {error, not_found} |
    {ok, value()}.
get(Key, Options) ->
    case proplists:get_value(refresh_function, Options, undefined) of
        undefined ->
            get_from_cache(Key);
        Closure when is_function(Closure) ->
            get_and_maybe_refresh(Key, Closure, Options);
        {Module, Function} when is_atom(Module), is_atom(Function) ->
            ?debugFmt("get and maybe refresh hit", []),
            get_and_maybe_refresh(Key, {Module, Function}, Options)
    end.

-spec set(key(), value()) ->
    ok |
    {error, invalid_ttl} |
    {error, invalid_evict}.
set(Key, Value) ->
    set(Key, Value, []).

-spec set(key(), value(), cache_options()) ->
    ok |
    {error, invalid_ttl} |
    {error, invalid_evict}.
set(Key, Value, Options) ->
    GetTtl = get_numeric_value(Options, ttl),
    GetEvict = get_numeric_value(Options, evict),
    case {GetTtl, GetEvict} of
        {{ok, Ttl}, {ok, Evict}} ->
            erl_cache_server:set(Key, Value, Ttl, Evict);
        {{error, ttl}, _} ->
            {error, invalid_ttl};
        {_, {error, evict}} ->
            {error, invalid_evict}
    end.

-spec evict(key()) -> ok.
evict(Key) ->
    erl_cache_server:evict(Key).

%% ====================================================================
%% private
%% ====================================================================

-spec get_from_cache(key()) ->
    {error, not_found} |
    {ok, value()}.
get_from_cache(Key) ->
    case erl_cache_server:get(Key) of
        {miss} ->
            {error, not_found};
        {evict, _} ->
            erl_cache_server:evict(Key),
            {error, not_found};
        {stale, Value} ->
            {ok, Value};
        {hit, Value} ->
            {ok, Value}
    end.

-spec get_and_maybe_refresh(key(), refresh_function(), cache_options()) ->
    {ok, value()} |
    {error, not_found} |
    {error, invalid_refresh}.
get_and_maybe_refresh(Key, Fun, Options) ->
    case erl_cache_server:get(Key) of
        {miss} ->
            maybe_refresh(Key, undefined, Fun, Options, miss);
        {evict, _} ->
            maybe_refresh(Key, undefined, Fun, Options, evict);
        {stale, Value} ->
            ?debugFmt("The value was stale", []),
            maybe_refresh(Key, {ok, Value}, Fun, Options, stale);
        {hit, Value} ->
            {ok, Value}
    end.

-spec maybe_refresh(key(), value(), refresh_function(), cache_options(), miss | evict | stale) ->
    {ok, value()} |
    {error, not_found} |
    {error, invalid_refresh}.
maybe_refresh(Key, Value, Fun, Options, State) ->
    GetRefresh = get_refresh_strategy(Options, State),
    case GetRefresh of
        {ok, never} ->
            value_or_not_found(Value);
        {ok, synchronous} ->
            ?debugFmt("Invoking sync set", []),
            sync_set(Key, Fun, Options);
        {ok, asynchronous} ->
            async_set(Key, Fun, Options),
            value_or_not_found(Value);
        {error, invalid_refresh} ->
            {error, invalid_refresh}
    end.

-spec get_refresh_strategy(cache_options(), miss | stale | evict) ->
    {ok, refresh_options()} |
    {error, invalid_refresh}.
get_refresh_strategy(Options, State) ->
    {OptionsKey, DefaultKey, FallbackValue} = case State of
        miss ->
            {refresh_miss, default_refresh_miss, ?DEFAULT_REFRESH_MISS};
        stale ->
            {refresh_stale, default_refresh_stale, ?DEFAULT_REFRESH_STALE};
        evict ->
            {refresh_evict, default_refresh_evict, ?DEFAULT_REFRESH_EVICT}
    end,
    ?debugFmt("Opts=~p, OptsKey= ~p", [Options, OptionsKey]),
    case proplists:get_value(OptionsKey, Options, default) of
        default ->
            case application:get_env(erl_cache, DefaultKey) of
                undefined ->
                    {ok, FallbackValue};
                {ok, Refresh} when Refresh =:= never;
                        Refresh =:= synchronous;
                        Refresh =:= asynchronous ->
                    {ok, Refresh};
                _R ->
                    {error, invalid_refresh}
            end;
        Refresh when Refresh =:= never;
                Refresh =:= synchronous;
                Refresh =:= asynchronous ->
            {ok, Refresh};
        _R ->
            {error, invalid_refresh}
    end.

-spec sync_set(key(), function(), cache_options()) ->
    {ok, value()} |
    {error, invalid_ttl} |
    {error, invalid_evict}.
sync_set(Key, Fun, Options) ->
    Value = compute_value(Fun, Options),
    case set(Key, Value, Options) of
        ok -> {ok, Value};
        {error, E} -> {error, E}
    end.

-spec async_set(key(), function(), cache_options()) -> ok.
async_set(Key, Fun, Options) ->
    spawn(fun() ->
        Value = compute_value(Fun, Options),
        set(Key, Value, Options)
    end),
    ok.

-spec compute_value(refresh_function(), cache_options()) -> value().
compute_value({Module, Function}, Options) ->
    Args = proplists:get_value(refresh_args, Options),
    erlang:apply(Module, Function, [Args]);
compute_value(Closure, Options) ->
    Args = proplists:get_value(refresh_args, Options),
    erlang:apply(Closure, [Args]).

-spec value_or_not_found(undefined | {ok, value()}) ->
    {error, not_found} |
    {ok, value()}.
value_or_not_found(undefined) ->
    {error, not_found};
value_or_not_found({ok, Value}) ->
    {ok, Value}.

-spec get_numeric_value(cache_options(), ttl|evict) ->
    {ok, non_neg_integer()} |
    {error, ttl | evict}.
get_numeric_value(Options, Type) ->
    {OptionsKey, DefaultKey, FallbackValue} = case Type of
        ttl ->
            {ttl, default_ttl, ?DEFAULT_TTL};
        evict ->
            {evict, default_evict, ?DEFAULT_EVICT}
    end,
    case proplists:get_value(OptionsKey, Options, default) of
        default ->
            case application:get_env(erl_cache, DefaultKey) of
                undefined ->
                    {ok, FallbackValue};
                {ok, Ttl} when Ttl >= 0 ->
                    {ok, Ttl};
                _ ->
                    {error, OptionsKey}
            end;
        Ttl when Ttl >= 0 ->
            {ok, Ttl};
        _ ->
            {error, OptionsKey}
    end.
