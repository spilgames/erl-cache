-module(erl_cache).

-include("erl_cache.hrl").

-export([
        get/1, get/2,
        set/2, set/3,
        evict/1
    ]).

-type key() :: term().
-type value() :: term().
-type refresh_function() :: function() | mfa() | undefined.
-type config_key()::validity | evict | refresh_callback | wait_for_refresh | wait_until_cached.

-type cache_get_option()::{wait_for_refresh, boolean()}.

-type cache_set_option() ::
    %% Mark entry as stale after X ms
    {validity, default | pos_integer()} |
    %% Evict stale entries after X ms
    {evict, default | non_neg_integer()} |
    %% Wait until the value has actually been set
    {wait_until_cached, boolean()} |
    %% Refresh stale, non-evicted entries with this strategy
    {refresh_callback, refresh_function()}.

-export_type([
    cache_set_option/0, cache_get_option/0,
    key/0, value/0, refresh_function/0
]).

%% ====================================================================
%% API
%% ====================================================================

-spec get(key()) ->
    {error, not_found} |
    {ok, value()}.
get(Key) ->
    get(Key, []).

-spec get(key(), [cache_get_option()]) ->
    {error, not_found} |
    {error, {invalid, wait_for_refresh}} |
    {ok, value()}.
get(Key, Options) ->
    case validate_value(wait_for_refresh, Options) of
        {ok, Wait} ->
            {ok, Res} = erl_cache_server:get(Key, Wait),
            ?DEBUG("get(~p) = ~p", [Key, Res]),
            case Res of
                evicted -> {error, not_found};
                miss -> {error, not_found};
                {_, Val} -> {ok, Val}
            end;
        {error, _}=E -> E
    end.

-spec set(key(), value()) ->
    ok |
    {error, invalid_validity} |
    {error, invalid_evict}.
set(Key, Value) ->
    set(Key, Value, []).

-spec set(key(), value(), [cache_set_option()]) ->
    ok |
    {error, invalid_validity} |
    {error, invalid_evict}|
    {error, invalid_callback}.
set(Key, Value, Options) ->
    ParsedValidity = validate_value(validity, Options),
    ParsedEvict = validate_value(evict, Options),
    ParsedCb = validate_value(refresh_callback, Options),
    ParsedWait = validate_value(wait_until_cached, Options),
    case {ParsedValidity, ParsedEvict, ParsedCb, ParsedWait} of
        {{ok, Validity}, {ok, Evict}, {ok, Cb}, {ok, Wait}} ->
            erl_cache_server:set(Key, Value, Validity, Evict, Cb, Wait);
        T when is_tuple(T) ->
            [H|_] = lists:dropwhile(fun ({Type, _}) -> Type/=error end, tuple_to_list(T)),
            H
    end.

-spec evict(key()) -> ok.
evict(Key) ->
    erl_cache_server:evict(Key).

%% ====================================================================
%% private
%% ====================================================================

-spec validate_value(config_key(), [cache_set_option()]) ->
    {ok, non_neg_integer()} |
    {error, {invalid, config_key()}}.
validate_value(Key, Opts) when Key==refresh_callback ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> {ok, undefined};
        {M, F, A} when is_atom(M) andalso is_atom(F) andalso is_list(A) -> {ok, {M, F, A}};
        Fun when is_function(Fun) -> {ok, Fun};
        _ -> {error, {invalid, Key}}
    end;
validate_value(Key, Opts) when Key==validity; Key==evict ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> {ok, default(Key)};
        N when is_integer(N) andalso N>0 -> {ok, N};
        _ -> {error, {invalid, Key}}
    end;
validate_value(Key, Opts) when Key==wait_for_refresh; Key==wait_until_cached ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> {ok, default(Key)};
        B when is_boolean(B) -> {ok, B};
        _ -> {error, {invalid, Key}}
    end.

-spec default(validity | evict) -> pos_integer().
default(validity) -> get_env(validity, ?DEFAULT_VALIDITY);
default(evict) -> get_env(evict, ?DEFAULT_EVICT);
default(wait_for_refresh) -> get_env(wait_for_refresh, ?DEFAULT_WAIT_FOR_REFRESH);
default(wait_until_cached) -> get_env(wait_until_cached, ?DEFAULT_WAIT_UNTIL_CACHED).

-spec get_env(atom(), term()) -> term().
get_env(Key, Default) ->
    case application:get_env(erl_cache, Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end.
