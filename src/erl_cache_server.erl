-module(erl_cache_server).

-behaviour(gen_server).

-include("erl_cache.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/1,
    get/3,
    set/7,
    evict/3,
    get_stats/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    name::erl_cache:name(),  %% The name of this cache instance
    cache::ets:tid(),        %% Holds cache
    stats::dict()            %% Statistics about cache hits
}).

-record(cache_entry, {
    key::term(),
    value::term(),
    created::pos_integer(),
    validity::pos_integer(),
    evict::pos_integer(),
    validity_delta::pos_integer(),
    evict_delta::pos_integer(),
    refresh_callback::erl_cache:refresh_function()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts the server
-spec start_link(erl_cache:name()) -> {ok, pid()}.
%% @end
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

-spec get(erl_cache:name(), erl_cache:key(), boolean()) ->
    {ok, erl_cache:value()} | {error, not_found}.
get(Name, Key, WaitForRefresh) ->
    Now = now_ms(),
    case ets:lookup(get_table_name(Name), Key) of
        [#cache_entry{validity=Validity, value=Value}] when Now < Validity ->
            gen_server:cast(Name, {increase_stat, hit}),
            {ok, Value};
        [#cache_entry{evict=Evict, value=Value, refresh_callback=undefined}] when Now < Evict ->
            gen_server:cast(Name, {increase_stat, stale}),
            {ok, Value};
        [#cache_entry{evict=Evict, refresh_callback=Cb}=Entry] when Now < Evict, Cb /=undefined ->
            ?DEBUG("Refreshing stale key ~p", [Key]),
            gen_server:cast(Name, {increase_stat, stale}),
            {ok, NewVal} = refresh(Name, Entry, WaitForRefresh),
            {ok, NewVal};
        [#cache_entry{value=_ExpiredValue}] ->
            evict(Name, Key, false),
            {error, not_found};
        [] ->
            gen_server:cast(Name, {increase_stat, miss}),
            {error, not_found}
    end.

-spec set(erl_cache:name(), erl_cache:key(), erl_cache:value(), pos_integer(), non_neg_integer(),
          erl_cache:refresh_function(), boolean()) -> ok.
set(Name, Key, Value, ValidityDelta, EvictDelta, RefreshCb, WaitTillSet) ->
    Now = now_ms(),
    Entry = #cache_entry{
        key = Key,
        value = Value,
        created = Now,
        validity = Now + ValidityDelta,
        evict = Now + ValidityDelta + EvictDelta,
        validity_delta = ValidityDelta,
        evict_delta = EvictDelta,
        refresh_callback = RefreshCb
    },
    case WaitTillSet of
        false -> ok = gen_server:cast(Name, {set, Entry});
        true -> ok = gen_server:call(Name, {set, Entry})
    end.

-spec evict(erl_cache:name(), erl_cache:key(), boolean()) -> ok.
evict(Name, Key, false) ->
    gen_server:cast(Name, {evict, Key});
evict(Name, Key, true) ->
    gen_server:call(Name, {evict, Key}).

%% @doc Retrieve stats for cache server usage. Great for testing and
%% to ensure nothing is broken.
-spec get_stats(erl_cache:name()) -> erl_cache:cache_stats().
%% @end
get_stats(Name) ->
    Info = ets:info(get_table_name(Name)),
    Memory = proplists:get_value(memory, Info, 0),
    Entries = proplists:get_value(size, Info, 0),
    ServerStats = gen_server:call(Name, get_stats),
    [{entries, Entries}, {memory, Memory}] ++ ServerStats.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([{name, erl_cache:name()}]) -> {ok, #state{}}.
init([Name]) ->
    CacheTid = ets:new(get_table_name(Name), [set, protected, named_table, {keypos,2},
                                              {read_concurrency, true}]),
    {ok, #state{name=Name, cache=CacheTid, stats=dict:new()}}.

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

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), #state{}) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal Function Definitions
%% ====================================================================

-spec set_cache_entry(ets:tid(), #cache_entry{}, dict()) -> dict().
set_cache_entry(Ets, Entry, Stats) ->
    ets:insert(Ets, Entry),
    update_stats(set, Stats).

-spec evict_cache_entry(ets:tid(), #cache_entry{}, dict()) -> dict().
evict_cache_entry(Ets, Key, Stats) ->
    ets:delete(Ets, Key),
    update_stats(evict, Stats).

-spec refresh(erl_cache:name(), #cache_entry{}, boolean()) ->
    {ok, erl_cache:value()}.
refresh(Name, #cache_entry{key=Key, validity_delta=ValidityDelta, evict_delta=EvictDelta,
                           refresh_callback=Callback}, true) when Callback/=undefined ->
    NewVal = do_apply(Callback),
    ok = set(Name, Key, NewVal, ValidityDelta, EvictDelta, Callback, true),
    {ok, NewVal};
refresh(Name, #cache_entry{key=Key, value=Value, validity_delta=ValidityDelta, evict_delta=EvictDelta,
                           refresh_callback=Callback}, false) when Callback/=undefined ->
    F = fun () ->
            NewVal = do_apply(Callback),
            set(Name, Key, NewVal, ValidityDelta, EvictDelta, Callback, false)
    end,
    _ = spawn(F),
    {ok, Value}.

-spec do_apply(mfa() | function()) -> term().
do_apply({M, F, A}) when is_atom(M), is_atom(F), is_list(A) ->
    apply(M, F, A);
do_apply(F) when is_function(F) ->
    F().

-spec update_stats(hit|miss|stale|evict, dict()) -> dict().
update_stats(Stat, Stats) ->
    dict:update_counter(total_ops, 1, dict:update_counter(Stat, 1, Stats)).

-spec now_ms() -> pos_integer().
now_ms() ->
    {Mega, Sec, Micro} = os:timestamp(),
    Mega * 1000000000 + Sec * 1000 + Micro div 1000.

-spec get_table_name(erl_cache:name()) -> ets:tid().
get_table_name(Name) ->
    to_atom(atom_to_list(Name) ++ "_ets").

-spec to_atom(string()) -> atom().
to_atom(Str) ->
    try list_to_existing_atom(Str) catch error:badarg -> list_to_atom(Str) end.

