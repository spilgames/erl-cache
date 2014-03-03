-module(erl_cache_server).

-behaviour(gen_server).

-include("erl_cache.hrl").
-include("logging.hrl").

%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([
    start_link/2,
    get/3,
    is_valid_name/1,
    set/9,
    evict/3,
    get_stats/1
]).

%% ==================================================================
%% gen_server Function Exports
%% ==================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    name::erl_cache:name(),                     %% The name of this cache instance
    cache::ets:tid(),                           %% Holds cache
    evict_interval::erl_cache:evict_interval(), %% How often expired entries are evicted
                                                %% from the CACHE table
    stats::dict()                               %% Statistics about cache hits
}).

-record(cache_entry, {
    key::erl_cache:key() | '_',
    value::erl_cache:value() | '_',
    created::pos_integer() | '_',
    validity::pos_integer() | '_',
    evict::pos_integer() | '$1',
    validity_delta::erl_cache:validity() | '_',
    error_validity_delta::erl_cache:error_validity() | '_',
    evict_delta::erl_cache:evict() | '_',
    refresh_callback::erl_cache:refresh_callback() | '_',
    is_error_callback::erl_cache:is_error_callback() | '_'
}).

%% ==================================================================
%% API Function Definitions
%% ==================================================================

-spec start_link(erl_cache:name(), erl_cache:evict_interval()) -> {ok, pid()}.
start_link(Name, EvictInterval) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, EvictInterval}, []).

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
    set(Name, Entry, WaitTillSet).

-spec evict(erl_cache:name(), erl_cache:key(), erl_cache:wait_until_done()) -> ok.
evict(Name, Key, false) ->
    gen_server:cast(Name, {evict, Key});
evict(Name, Key, true) ->
    gen_server:call(Name, {evict, Key}).

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
-spec init({erl_cache:name(), erl_cache:evict_interval()}) -> {ok, #state{}}.
init({Name, EvictInterval}) ->
    CacheTid = ets:new(get_table_name(Name), [set, protected, named_table, {keypos,2},
                                              {read_concurrency, true}]),
    {ok, _} = timer:send_after(EvictInterval, Name, purge_cache),
    {ok, #state{name=Name, evict_interval=EvictInterval, cache=CacheTid, stats=dict:new()}}.

%% @private
-spec handle_call(term(), term(), #state{}) ->
    {reply, Data::any(), #state{}}.
handle_call({set, #cache_entry{}=Entry}, _From, #state{cache=Ets, stats=StatsDict} = State) ->
    UpdatedStats = set_cache_entry(Ets, Entry, StatsDict),
    {reply, ok, State#state{stats=UpdatedStats}};
handle_call({evict, Key}, _From, #state{cache=Ets, stats=StatsDict} = State) ->
    UpdatedStats = evict_cache_entry(Ets, Key, StatsDict),
    {reply, ok, State#state{stats=UpdatedStats}};
handle_call(get_stats, _From, #state{stats=Stats} = State) ->
    {reply, dict:to_list(Stats), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast({increase_stat, Stat}, #state{stats=Stats} = State) ->
    {noreply, State#state{stats=update_stats(Stat, Stats)}};
handle_cast({set, #cache_entry{}=Entry}, #state{cache=Ets, stats=StatsDict} = State) ->
    UpdatedStats = set_cache_entry(Ets, Entry, StatsDict),
    {noreply, State#state{stats=UpdatedStats}};
handle_cast({evict, Key}, #state{cache=Ets, stats=StatsDict} = State) ->
    UpdatedStats = evict_cache_entry(Ets, Key, StatsDict),
    {noreply, State#state{stats=UpdatedStats}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(purge_cache,
            #state{name=Name, stats=Stats, cache=Ets, evict_interval=EvictInterval}=State) ->
    Now = now_ms(),
    {Time, Deleted} = timer:tc(
        ets, select_delete, [Ets, [{#cache_entry{evict='$1', _='_'}, [{'<', '$1', Now}], [true]}]]),
    ?INFO("~p cache purged in ~pms", [Name, Time]),
    UpdatedStats = update_stats(evict, Deleted, Stats),
    {ok, _} = timer:send_after(EvictInterval, Name, purge_cache),
    {noreply, State#state{stats=UpdatedStats}};
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
-spec set(erl_cache:name(), #cache_entry{}, erl_cache:wait_until_done()) -> ok.
set(Name, Entry, true) ->
    ok = gen_server:call(Name, {set, Entry});
set(Name, Entry, false) ->
    ok = gen_server:cast(Name, {set, Entry}).

%% @private
-spec set_cache_entry(ets:tid(), #cache_entry{}, dict()) -> dict().
set_cache_entry(Ets, Entry, Stats) ->
    ets:insert(Ets, Entry),
    update_stats(set, Stats).

%% @private
-spec evict_cache_entry(ets:tid(), #cache_entry{}, dict()) -> dict().
evict_cache_entry(Ets, Key, Stats) ->
    ets:delete(Ets, Key),
    update_stats(evict, Stats).

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
    ok = set(Name, RefreshedEntry, WaitForRefresh),
    NewVal.

%% @private
-spec do_apply({module(), atom(), [term()]} | function()) -> term().
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
-spec update_stats(hit|miss|overdue|evict|set, dict()) -> dict().
update_stats(Stat, Stats) ->
    update_stats(Stat, 1, Stats).

%% @private
-spec update_stats(hit|miss|overdue|evict|set, pos_integer(), dict()) -> dict().
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

