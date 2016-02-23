-module(erl_cache_server).

-behaviour(gen_server).

-include("erl_cache.hrl").
-include("logging.hrl").

%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([
    start_link/1,
    get/3,
    is_valid_name/1,
    set/9,
    evict/3,
    check_mem_usage/1,
    get_stats/1
]).

%% ==================================================================
%% gen_server Function Exports
%% ==================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(namespace_types).
-type stats_dict() :: dict:dict().
-else.
-type stats_dict() :: dict().
-endif.


-record(state, {
    name::erl_cache:name(),                     %% The name of this cache instance
    cache::ets:tid(),                           %% Holds cache
    stats::stats_dict()                         %% Statistics about cache hits
}).

-record(cache_entry, {
    key::erl_cache:key(),
    value::erl_cache:value(),
    created::pos_integer(),
    validity::pos_integer(),
    evict::pos_integer(),
    validity_delta::erl_cache:validity(),
    error_validity_delta::erl_cache:error_validity(),
    evict_delta::erl_cache:evict(),
    refresh_callback::erl_cache:refresh_callback(),
    is_error_callback::erl_cache:is_error_callback()
}).

%% ==================================================================
%% API Function Definitions
%% ==================================================================

-spec start_link(erl_cache:name()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Name, []).

-spec get(erl_cache:name(), erl_cache:key(), erl_cache:wait_for_refresh()) ->
    {ok, erl_cache:value()} | {error, not_found}.
get(Name, Key, WaitForRefresh) ->
    Now = now_ms(),
    case ets:lookup(get_table_name(Name), Key) of
        [#cache_entry{validity=Validity, value=Value}] when Now < Validity ->
            gen_server:cast(Name, {increase_stat, hit}),
            {ok, Value};
        [#cache_entry{evict=Evict, value=Value, refresh_callback=undefined}] when Now < Evict ->
            gen_server:cast(Name, {increase_stat, overdue}),
            {ok, Value};
        [#cache_entry{evict=Evict, refresh_callback=Cb}=Entry] when Now < Evict, Cb /=undefined ->
            ?DEBUG("Refreshing overdue key ~p", [Key]),
            gen_server:cast(Name, {increase_stat, overdue}),
            {ok, NewVal} = refresh(Name, Entry, WaitForRefresh),
            {ok, NewVal};
        [#cache_entry{value=_ExpiredValue}] ->
            {error, not_found};
        [] ->
            gen_server:cast(Name, {increase_stat, miss}),
            {error, not_found}
    end.

-spec set(erl_cache:name(), erl_cache:key(), erl_cache:value(), pos_integer(), non_neg_integer(),
          erl_cache:refresh_callback(), erl_cache:wait_until_done(), erl_cache:error_validity(),
          erl_cache:is_error_callback()) -> ok.
set(Name, Key, Value, ValidityDelta, EvictDelta,
    RefreshCb, WaitTillSet, ErrorValidityDelta, IsErrorCb) ->
    Now = now_ms(),
    {Validity, Evict} = case is_error_value(IsErrorCb, Value) of
        false -> {Now + ValidityDelta, Now + ValidityDelta + EvictDelta};
        true -> {Now + ErrorValidityDelta, Now + ErrorValidityDelta}
    end,
    Entry = #cache_entry{
        key = Key,
        value = Value,
        created = Now,
        validity = Validity,
        error_validity_delta = ErrorValidityDelta,
        evict = Evict,
        validity_delta = ValidityDelta,
        evict_delta = EvictDelta,
        refresh_callback = RefreshCb,
        is_error_callback = IsErrorCb
    },
    operate_cache(Name, fun do_set/2, [Name, Entry], set, WaitTillSet).

-spec evict(erl_cache:name(), erl_cache:key(), erl_cache:wait_until_done()) -> ok.
evict(Name, Key, WaitUntilDone) ->
    operate_cache(Name, fun do_evict/2, [Name, Key], evict, WaitUntilDone).

-spec get_stats(erl_cache:name()) -> erl_cache:cache_stats().
get_stats(Name) ->
    Info = ets:info(get_table_name(Name)),
    Memory = proplists:get_value(memory, Info, 0),
    Entries = proplists:get_value(size, Info, 0),
    ServerStats = gen_server:call(Name, get_stats),
    [{entries, Entries}, {memory, Memory}] ++ ServerStats.

-spec is_valid_name(erl_cache:name()) -> boolean().
is_valid_name(Name) ->
    not lists:member(get_table_name(Name), ets:all()).

%% ==================================================================
%% gen_server Function Definitions
%% ==================================================================

%% @private
-spec init(erl_cache:name()) -> {ok, #state{}}.
init(Name) ->
    CacheTid = ets:new(get_table_name(Name), [set, public, named_table, {keypos,2},
                                              {read_concurrency, true},
                                              {write_concurrency, true}]),
    EvictInterval = erl_cache:get_cache_option(Name, evict_interval),
    {ok, _} = timer:send_after(EvictInterval, Name, purge_cache),
    MemCheckInterval = erl_cache:get_cache_option(Name, mem_check_interval),
    {ok, _} = timer:apply_after(MemCheckInterval, ?MODULE, check_mem_usage, [Name]),
    {ok, #state{name=Name, cache=CacheTid, stats=dict:new()}}.

%% @private
-spec handle_call(term(), term(), #state{}) ->
    {reply, Data::any(), #state{}}.
handle_call(get_stats, _From, #state{stats=Stats} = State) ->
    {reply, dict:to_list(Stats), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast({increase_stat, Stat}, #state{stats=Stats} = State) ->
    {noreply, State#state{stats=update_stats(Stat, Stats)}};
handle_cast({increase_stat, Stat, N}, #state{stats=Stats} = State) ->
    {noreply, State#state{stats=update_stats(Stat, N, Stats)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(purge_cache, #state{name=Name}=State) ->
    purge_cache(Name),
    EvictInterval = erl_cache:get_cache_option(Name, evict_interval),
    {ok, _} = timer:send_after(EvictInterval, Name, purge_cache),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(any(), #state{}) -> any().
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal Function Definitions
%% ====================================================================

%% @private
-spec operate_cache(erl_cache:name(), function(), list(), atom(), boolean()) -> ok.
operate_cache(Name, Function, Input, Stat, Sync) ->
    case Sync of
        true -> apply(Function, Input);
        false -> spawn_link(erlang, apply, [Function, Input])
    end,
    gen_server:cast(Name, {increase_stat, Stat}).

%% @private
-spec do_set(erl_cache:name(), #cache_entry{}) -> ok.
do_set(Name, Entry) ->
    true = ets:insert(get_table_name(Name), Entry),
    ok.

%% @private
-spec do_evict(erl_cache:name(), erl_cache:key()) -> ok.
do_evict(Name, Key) ->
    true = ets:delete(get_table_name(Name), Key),
    ok.

%% @private
-spec purge_cache(erl_cache:name()) -> ok.
purge_cache(Name) ->
    Now = now_ms(),
    {Time, Deleted} = timer:tc(
        ets, select_delete,
        [get_table_name(Name), [{#cache_entry{evict='$1', _='_'}, [{'<', '$1', Now}], [true]}]]),
    ?INFO("~p cache purged in ~pms", [Name, Time]),
    gen_server:cast(Name, {increase_stat, evict, Deleted}),
    ok.


%% @private
-spec refresh(erl_cache:name(), #cache_entry{}, erl_cache:wait_for_refresh()) ->
    {ok, erl_cache:value()}.
refresh(Name, #cache_entry{refresh_callback=Callback}=Entry, true) when Callback/=undefined ->
    NewVal = do_refresh(Name, Entry, true),
    {ok, NewVal};
refresh(Name, #cache_entry{value=Value, refresh_callback=Callback}=Entry, false)
        when Callback/=undefined ->
    F = fun () -> do_refresh(Name, Entry, false) end,
    _ = spawn(F),
    {ok, Value}.

%% @private
-spec do_refresh(erl_cache:name(), #cache_entry{}, erl_cache:wait_for_refresh()) ->
    erl_cache:value().
do_refresh(Name, #cache_entry{key=Key, validity_delta=ValidityDelta, evict_delta=EvictDelta,
                              refresh_callback=Callback, is_error_callback=IsErrorCb}=Entry,
           WaitForRefresh) ->
    NewVal = do_apply(Callback),
    Now = now_ms(),
    RefreshedEntry = case is_error_value(IsErrorCb, NewVal) of
        false ->
            Entry#cache_entry{value=NewVal, validity=Now+ValidityDelta,
                              evict=Now+ValidityDelta+EvictDelta};
        true ->
            ?NOTICE("Error refreshing ~p at ~p: ~p. Disabling auto refresh...",
                    [Key, Name, NewVal]),
            Entry#cache_entry{refresh_callback=undefined}
    end,
    ok = operate_cache(Name, fun do_set/2, [Name, RefreshedEntry], set, WaitForRefresh),
    NewVal.

%% @private
-spec check_mem_usage(erl_cache:name()) -> ok.
check_mem_usage(Name) ->
    MaxMB = erl_cache:get_cache_option(Name, max_cache_size),
    CurrentWords = proplists:get_value(memory, ets:info(get_table_name(Name))),
    CurrentMB = erlang:trunc((CurrentWords * erlang:system_info(wordsize)) / (1024*1024*8)),
    case MaxMB /= undefined andalso CurrentMB > MaxMB of
        true ->
            ?WARNING("~p exceeded memory limit of ~pMB: ~pMB in use! Forcing eviction...",
                     [Name, MaxMB, CurrentMB]),
            purge_cache(Name);
        false -> ok
    end,
    MemCheckInterval = erl_cache:get_cache_option(Name, mem_check_interval),
    {ok, _} = timer:apply_after(MemCheckInterval, ?MODULE, check_mem_usage, [Name]),
    ok.

%% @private
-spec do_apply(mfa() | function()) -> term().
do_apply({M, F, A}) when is_atom(M), is_atom(F), is_list(A) ->
    apply(M, F, A);
do_apply(F) when is_function(F) ->
    F().

%% @private
-spec is_error_value(erl_cache:is_error_callback(), erl_cache:value()) -> boolean().
is_error_value({M, F, A}, Value) ->
    apply(M, F, [Value|A]);
is_error_value(F, Value) when is_function(F) ->
    F(Value).

%% @private
-spec update_stats(hit|miss|overdue|evict|set, stats_dict()) -> stats_dict().
update_stats(Stat, Stats) ->
    update_stats(Stat, 1, Stats).

%% @private
-spec update_stats(hit|miss|overdue|evict|set, pos_integer(), stats_dict()) -> stats_dict().
update_stats(Stat, N, Stats) ->
    dict:update_counter(total_ops, N, dict:update_counter(Stat, N, Stats)).

%% @private
-spec now_ms() -> pos_integer().
now_ms() ->
    {Mega, Sec, Micro} = os:timestamp(),
    Mega * 1000000000 + Sec * 1000 + Micro div 1000.

%% @private
-spec get_table_name(erl_cache:name()) -> atom().
get_table_name(Name) ->
    to_atom(atom_to_list(Name) ++ "_ets").

%% @private
-spec to_atom(string()) -> atom().
to_atom(Str) ->
    try list_to_existing_atom(Str) catch error:badarg -> list_to_atom(Str) end.

