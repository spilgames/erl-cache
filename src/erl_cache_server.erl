-module(erl_cache_server).

-behaviour(gen_server).

-include_lib("erlanglibs/include/logging.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/1,
    set/4,
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
    stats::dict()                           %% Statistics about cache hits
}).

-record(entry, {
    key::term(),
    value::term(),
    created::pos_integer(),
    ttl::pos_integer(),
    evict::pos_integer()
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

-spec get(erl_cache_facade:key()) ->
    {hit, erl_cache_facade:value()} |
    {stale, erl_cache_facade:value()} |
    {evict, erl_cache_facade:value()} |
    {miss}.
get(Key) ->
    Now = elibs_time:now(),
    case ets:lookup(?ETS_CACHE_TABLE, Key) of
        [#entry{ttl=Ttl, value=Value}] when Now < Ttl ->
            update_cache_stats(Key, hit),
            {hit, Value};
        [#entry{evict=Evict, value=Value}] when Now < Evict ->
            update_cache_stats(Key, stale),
            {stale, Value};
        [#entry{value=Value}] ->
            update_cache_stats(Key, evict),
            {evict, Value};
        [] ->
            update_cache_stats(Key, miss),
            {miss}
    end.

-spec set(erl_cache_facade:key(), erl_cache_facade:value(), pos_integer(), non_neg_integer()) -> ok.
set(Key, Value, TtlDelta, EvictDelta) ->
    Now = elibs_time:now(),
    Ttl = Now + TtlDelta,
    Evict = Ttl + EvictDelta,
    Entry = #entry{
        key=Key,
        value=Value,
        created=Now,
        ttl=Ttl,
        evict=Evict
    },
    gen_server:cast(?SERVER, {set, Entry}).

-spec evict(erl_cache_facade:key()) -> ok.
evict(Key) ->
    gen_server:cast(?SERVER, {evict, Key}).


%% @doc Retrieve stats for cache server usage. Great for testing and
%% to ensure nothing is broken. Obviously doesn't work if the application
%% was not yet started (since the ETS would not have been created).
-spec get_stats() -> proplists:proplist().
%% @end
get_stats() ->
    Info = ets:info(?ETS_CACHE_TABLE),
    Memory = proplists:get_value(memory, Info, 0),
    Entries = proplists:get_value(size, Info, 0),
    %Keys = lists:flatten(lists:usort(ets:match(?ETS_CACHE_TABLE,{'$1','_'}))),    %ensure asc order
    %Stats = gen_server:call(?MODULE, get_stats, 500),
    [{entries, Entries}, {memory, Memory}].

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
handle_call(get_stats, _From, #state{stats=Stats} = State) ->
    {reply, dict:to_list(Stats), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast({set_stats, Key, Stats}, #state{stats=StatsDict} = State) ->
    {noreply, State#state{
        stats=update_stats(Key, StatsDict, Stats)
    }};
handle_cast({set, #entry{key=Key}=Entry}, #state{stats=StatsDict} = State) ->
    ets:insert(?ETS_CACHE_TABLE, Entry),
    {noreply, State#state{
        stats=update_stats(Key, StatsDict, [{cache, set}])
    }};
handle_cast({evict, Key}, #state{stats=StatsDict} = State) ->
    ets:delete(?ETS_CACHE_TABLE, Key),
    {noreply, State#state{
        stats=update_stats(Key, StatsDict, [{cache, evict}])
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

%% ------------------------------------------------------------------
%% CACHE: labels_per_application
%% ------------------------------------------------------------------

%% @doc Update the stats dictionary. Per site keeps track of cached/expired stats.
-spec update_stats(term(), dict(), proplists:proplist()) -> dict().
%% @end
update_stats(_, Dict, []) ->
    Dict;
update_stats(Key, Dict, [Stat|RestStats]) ->
    NewDict = dict:update_counter({Key, Stat}, 1, Dict),
    update_stats(Key, NewDict, RestStats).


%% @doc Update the stats dictionary. Per site keeps track of hit/miss stats.
%% IMPORTANT: only enabled for TEST purposes.
-spec update_cache_stats(term(), hit|miss|stale|set|evict) -> ok.
%% @end
update_cache_stats(Key, Result) ->
    ?DEBUG("erl_cache ~p for key=~p", [Result, Key]),
    gen_server:cast(?SERVER, {set_stats, Key, [{cache, Result}]}),
    ok.
