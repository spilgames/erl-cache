-module(erl_cache_server).

-behaviour(gen_server).

-include("erl_cache.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    set/6,
    evict/1,
    get_stats/0
]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
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

-define(ETS_CACHE_TABLE, erl_cache_server_table).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts the server
-spec start_link() -> {ok, pid()}.
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(erl_cache:key(), boolean()) ->
    {ok, {hit, erl_cache:value()} | {stale, erl_cache:value()} | evicted | miss}.
get(Key, WaitForRefresh) ->
    Now = now_ms(),
    Res = case ets:lookup(?ETS_CACHE_TABLE, Key) of
        [#cache_entry{validity=Validity, value=Value}] when Now < Validity ->
            gen_server:cast(?MODULE, {increase_stat, hit}),
            {hit, Value};
        [#cache_entry{evict=Evict, value=Value, refresh_callback=undefined}] when Now < Evict ->
            gen_server:cast(?MODULE, {increase_stat, stale}),
            {stale, Value};
        [#cache_entry{evict=Evict, refresh_callback=Cb}=Entry] when Now < Evict, Cb /=undefined ->
            ?DEBUG("Refreshing stale key ~p", [Key]),
            gen_server:cast(?MODULE, {increase_stat, stale}),
            {ok, NewVal} = refresh(Entry, WaitForRefresh),
            {stale, NewVal};
        [#cache_entry{value=_ExpiredValue}] ->
            evict(Key),
            evicted;
        [] ->
            gen_server:cast(?MODULE, {increase_stat, miss}),
            miss
    end,
    {ok, Res}.

-spec set(erl_cache:key(), erl_cache:value(), pos_integer(), non_neg_integer(),
          erl_cache:refresh_function(), boolean()) -> ok.
set(Key, Value, ValidityDelta, EvictDelta, RefreshCb, WaitTillSet) ->
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
        false -> ok = gen_server:cast(?SERVER, {set, Entry});
        true -> ok = gen_server:call(?SERVER, {set, Entry})
    end.

-spec evict(erl_cache:key()) -> ok.
evict(Key) ->
    gen_server:cast(?SERVER, {evict, Key}).

%% @doc Retrieve stats for cache server usage. Great for testing and
%% to ensure nothing is broken.
-spec get_stats() -> proplists:proplist().
%% @end
get_stats() ->
    Info = ets:info(?ETS_CACHE_TABLE),
    Memory = proplists:get_value(memory, Info, 0),
    Entries = proplists:get_value(size, Info, 0),
    ServerStats = gen_server:call(?SERVER, get_stats),
    [{entries, Entries}, {memory, Memory}] ++ ServerStats.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {ok, #state{}}.
init([]) ->
    CacheTid = ets:new(?ETS_CACHE_TABLE, [
            set, protected, named_table,
            {keypos,2},
            {read_concurrency, true}
        ]),
    {ok, #state{
        cache=CacheTid,
        stats=dict:new()
    }}.

-spec handle_call(term(), term(), #state{}) ->
    {reply, Data::any(), #state{}}.
handle_call({set, #cache_entry{}=Entry}, _From, #state{stats=StatsDict} = State) ->
    UpdatedStats = set_cache_entry(Entry, StatsDict),
    {reply, ok, State#state{stats=UpdatedStats}};
handle_call(get_stats, _From, #state{stats=Stats} = State) ->
    {reply, dict:to_list(Stats), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast({increase_stat, Stat}, #state{stats=Stats} = State) ->
    {noreply, State#state{
        stats=update_stats(Stat, Stats)
    }};
handle_cast({set, #cache_entry{}=Entry}, #state{stats=StatsDict} = State) ->
    UpdatedStats = set_cache_entry(Entry, StatsDict),
    {noreply, State#state{stats=UpdatedStats}};
handle_cast({evict, Key}, #state{stats=StatsDict} = State) ->
    ets:delete(?ETS_CACHE_TABLE, Key),
    {noreply, State#state{
        stats=update_stats(evict, StatsDict)
    }};
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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec set_cache_entry(#cache_entry{}, dict()) -> dict().
set_cache_entry(Entry, Stats) ->
    ets:insert(?ETS_CACHE_TABLE, Entry),
    update_stats(set, Stats).

-spec refresh(#cache_entry{}, boolean()) ->
    {ok, erl_cache:value()}.
refresh(#cache_entry{key=Key, validity_delta=ValidityDelta, evict_delta=EvictDelta,
               refresh_callback=Callback}, true) when Callback/=undefined ->
    NewVal = do_apply(Callback),
    ok = set(Key, NewVal, ValidityDelta, EvictDelta, Callback, true),
    {ok, NewVal};
refresh(#cache_entry{key=Key, value=Value, validity_delta=ValidityDelta, evict_delta=EvictDelta,
               refresh_callback=Callback}, false) when Callback/=undefined ->
    F = fun () ->
            NewVal = do_apply(Callback),
            set(Key, NewVal, ValidityDelta, EvictDelta, Callback, false)
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
