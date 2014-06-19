-module(erl_cache).

-behaviour(gen_server).

-include("erl_cache.hrl").
-include("logging.hrl").

%% ==================================================================
%% Escript API
%% ==================================================================
-export([main/1]).

%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([
        get/2, get/3,
        get_stats/1,
        list_cache_servers/0,
        set/3, set/4,
        start/0,
        start_link/0,
        stop_cache/1, start_cache/2,
        set_cache_defaults/2, get_cache_option/2,
        evict/2, evict/3
    ]).

-ignore_xref([
    {basho_bench, main, 1}
]).


-type name() :: atom(). %% The identifeir for a cache server
-type key() :: term().  %% The identifier for a cache entry
-type value() :: term().%% The value associated with a cache key

-type error_validity() :: validity(). %% When setting or refreshing a key with a value corresponding
                                      %% to an error according to the is_error_callback,
                                      %% error_validity will be used instead of validty and evict
                                      %% will be set to 0, forcing eviction after 2 failed refreshes
-type validity() :: pos_integer().  %% How long an entry shold be considered valid (in ms).
                                    %% Defaults to 300000
-type evict() :: non_neg_integer(). %% How long an entry shold be considered overdue (non valid byt
                                    %% not yet to be evicted, in ms). Defaults to 60000
-type evict_interval() :: pos_integer(). %% How often the cache should be scanned in order to delite
                                         %% expired entries
-type refresh_callback():: callback() | undefined. %% How to refresh a overdue entry when requested
                                                   %% via get. Defaults to undefined
-type is_error_callback():: callback(). %% fun((value()) -> boolean()) . When specified as an mfa(),
                                        %% the value to be verified will be prepended to the args
-type wait_for_refresh() :: boolean(). %% Whether a get call hiting a overdue value should wait for
                                       %% the refreshed value or return immediatly with a not_found
                                       %% Defaults to true
-type wait_until_done() :: boolean().  %% Whether set and evict operations should behave
                                       %% synchronously or asynchronously. Defaults to false

-type invalid_opt_error()::{invalid, config_key() | cache_name}.

-type key_generation_module()::atom(). %% A module implementing the erl_cache_key_generator behaviour.

-type cache_get_opt()::{wait_for_refresh, wait_for_refresh()}.

-type cache_size()::non_neg_integer(). %% Soft limit to the cache size in MB
-type cache_server_opt()::
    {max_cache_size, cache_size() | undefined} %% NOTE: this limit is soft and particularly
                                               %% innaccurate when working with large binaries!
    | {mem_check_interval, pos_integer()}.

-type cache_set_opt() ::
    {validity, validity()} |
    {evict, evict()} |
    {wait_until_done, wait_until_done()} |
    {key_generation, key_generation_module()} |
    {error_validity, validity()} |
    {is_error_callback, is_error_callback()} |
    {refresh_callback, refresh_callback()}.
-type cache_evict_opt() :: {wait_until_done, wait_until_done()}.
-type cache_opts()::[cache_get_opt() | cache_set_opt() | cache_evict_opt()
                     | {evict_interval, evict_interval()} | cache_server_opt()].

-type cache_stat()::{memory, pos_integer()} | {size, non_neg_integer()} | {hit, non_neg_integer()} |
                    {evict, non_neg_integer()} | {overdue, non_neg_integer()} |
                    {miss, non_neg_integer()}.
-type cache_stats()::[cache_stat()].

-type config_key()::validity | evict | refresh_callback | wait_for_refresh
                  | wait_until_done | evict_interval | is_error_callback | error_validity.

-type callback() :: function() | mfa().

-export_type([
        name/0, key/0, value/0, validity/0, evict/0, evict_interval/0, refresh_callback/0,
        cache_stats/0, wait_for_refresh/0, wait_until_done/0, error_validity/0, is_error_callback/0,
        cache_size/0, cache_opts/0
]).

%% ==================================================================
%% gen_server Function Exports
%% ==================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        cache_map::ets:tid()
}).

-define(CACHE_MAP, cache_map).
-define(SERVER, ?MODULE).


%% ====================================================================
%% Escript
%% ====================================================================
main(Args) ->
    basho_bench:main(Args).

%% ====================================================================
%% API
%% ====================================================================

%% @doc Convenience method to start the erl_cache application
-spec start() -> ok.
%% @end
start() ->
    ok = application:start(erl_cache).

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Starts a cache server with the given name and default options.
%% Returns a name in case the given name corresponds to a running cache server or any other process
%% in the system
-spec start_cache(name(), cache_opts()) -> ok | {error, invalid_opt_error()}.
%% @end
start_cache(Name, Opts) ->
    gen_server:call(?SERVER, {start_cache, Name, Opts}).

%% @doc Stops a cache server.
%% Returs an error if the given name does not correspond to a running cache server
-spec stop_cache(name()) -> ok | {error, invalid_opt_error()}.
%% @end
stop_cache(Name) ->
    gen_server:call(?SERVER, {stop_cache, Name}).

%% @doc Sets the defaults for a cache server.
-spec set_cache_defaults(name(), cache_opts()) ->  ok | {error, invalid_opt_error()}.
%% @end
set_cache_defaults(Name, CacheOpts) ->
    case is_available_name(Name) of
        false ->
            case validate_opts(CacheOpts, []) of
                {ok, ValidatedOpts} ->
                    gen_server:call(?SERVER, {set_defaults, Name, ValidatedOpts});
                {error, _}=E -> E
            end;
        true -> {error, {invalid, cache_name}}
    end.

%% @doc Gets the default value of a cache server option.
-spec get_cache_option(name(), cache_opts()) -> term().
%% @end
get_cache_option(Name, Opt) ->
    case ets:lookup(?CACHE_MAP, Name) of
        [] -> {error, {invalid, cache_name}};
        [{Name, Opts}] ->
            case Opt of
                evict_interval ->
                    proplists:get_value(evict_interval, arrange_evict_interval(Opts));
                _ ->
                    default(Opt, Opts)
            end
    end.

%% @see get/3
-spec get(name(), key()) ->
    {error, not_found} |
    {error, invalid_opt_error()} |
    {ok, value()}.
%% @end
get(Name, Key) ->
    get(Name, Key, []).

%% @doc Lists the names of the cache servers that have already been assigned
-spec list_cache_servers() -> [name()].
%% @end
list_cache_servers() ->
    ets:select(?CACHE_MAP, [{{'$1', '_'}, [], ['$1']}]).

%% @doc Gets the value associated with a given key in the cache signaled by the given name.
-spec get(name(), key(), [cache_get_opt()]) ->
    {error, not_found} |
    {error, invalid_opt_error()} |
    {ok, value()}.
%% @end
get(Name, Key, Opts) ->
    case validate_opts(Opts, get_name_defaults(Name)) of
        {ok, ValidatedOpts} ->
            erl_cache_server:get(Name, Key, proplists:get_value(wait_for_refresh, ValidatedOpts));
        {error, _}=E -> E
    end.

%% @doc Retrieves the stats associated with a cache instance
-spec get_stats(name()) -> {ok, cache_stats()}  | {error, invalid_opt_error()}.
%% @end
get_stats(Name) ->
    case is_cache_server(Name) of
        true -> erl_cache_server:get_stats(Name);
        false -> {error, {invalid, cache_name}}
    end.

%% @see set/4
-spec set(name(), key(), value()) -> ok | {error, invalid_opt_error()}.
%% @end
set(Name, Key, Value) ->
    set(Name, Key, Value, []).

%% @doc Sets a cache entry in a cache instance.
%% The options passed in this function call will overwrite the default ones for the cache instance
%% for any operation related to this specific key.
-spec set(name(), key(), value(), [cache_set_opt()]) -> ok | {error, invalid_opt_error()}.
%% @end
set(Name, Key, Value, Opts) ->
    case validate_opts(Opts, get_name_defaults(Name)) of
        {ok, ValidatedOpts} ->
            Validity = proplists:get_value(validity, ValidatedOpts),
            Evict = proplists:get_value(evict, ValidatedOpts),
            RefreshCb = proplists:get_value(refresh_callback, ValidatedOpts),
            Wait = proplists:get_value(wait_until_done, ValidatedOpts),
            ErrorValidity = proplists:get_value(error_validity, ValidatedOpts),
            IsErrorCb = proplists:get_value(is_error_callback, ValidatedOpts),
            erl_cache_server:set(Name, Key, Value, Validity, Evict,
                                 RefreshCb, Wait, ErrorValidity, IsErrorCb);
        {error, _}=E -> E
    end.

%% @see evict/3
-spec evict(name(), key()) -> ok  | {error, invalid_opt_error()}.
%% @end
evict(Name, Key) ->
    evict(Name, Key, []).

%% @doc Forces a cache entry to be evivted from the indicated cache instance
-spec evict(name(), key(), [cache_evict_opt()]) -> ok  | {error, invalid_opt_error()}.
%% @end
evict(Name, Key, Opts) ->
    case validate_opts(Opts, get_name_defaults(Name)) of
        {ok, ValidatedOpts} ->
            erl_cache_server:evict(Name, Key,
                                   proplists:get_value(wait_until_done, ValidatedOpts));
        {error, _}=E -> E
    end.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    Tid = ets:new(?CACHE_MAP, [set, protected, named_table, {read_concurrency, true}]),
    Servers = case application:get_env(erl_cache, cache_servers) of
        undefined -> [];
        {ok, L} when is_list(L) -> L
    end,
    ok = lists:foreach(fun ({Name, Opts}) -> do_start_cache(Name, Opts) end, Servers),
    {ok, #state{cache_map=Tid}}.


%% @private
-spec handle_call(term(), term(), #state{}) ->
    {reply, Data::any(), #state{}}.
handle_call({start_cache, Name, Defaults}, _From, #state{}=State) ->
    Res = do_start_cache(Name, Defaults),
    {reply, Res, State#state{}};
handle_call({stop_cache, Name}, _From, #state{}=State) ->
    Res = case is_cache_server(Name) of
        true ->
            ok = erl_cache_server_sup:remove_cache(Name),
            ?INFO("Stopping cache server '~p'", [Name]),
            true = ets:delete(?CACHE_MAP, Name),
            ok;
        false ->
            {error, {invalid, cache_name}}
    end,
    {reply, Res, State};
handle_call({set_defaults, Name, Opts}, _From, #state{}=State) ->
    true = ets:insert(?CACHE_MAP, {Name, Opts}),
    {reply, ok, State}.

%% @private
-spec do_start_cache(name(), cache_opts()) -> ok | {error, invalid_opt_error()}.
do_start_cache(Name, Opts) ->
    case is_available_name(Name) of
        true ->
            case validate_opts(Opts, []) of
                {ok, ValidatedOpts} ->
                    true = ets:insert(?CACHE_MAP, {Name, ValidatedOpts}),
                    ?INFO("Starting cache server '~p'", [Name]),
                    {ok, _} = erl_cache_server_sup:add_cache(Name),
                    ok;
                {error, _}=E -> E
            end;
        false ->
            {error, {invalid, cache_name}}
    end.

%% @private
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
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
%% Internal functions
%% ====================================================================

%% @private
-spec validate_opts(cache_opts(), cache_opts() | undefined) ->
    {ok, cache_opts()} | {error, invalid_opt_error()}.
validate_opts(_, undefined) ->
    {error, {invalid, cache_name}};
validate_opts(Opts, Defaults) ->
    CacheOpts = [validity, evict, refresh_callback, wait_for_refresh, max_cache_size,
                 wait_until_done, evict_interval, error_validity, is_error_callback,
                 mem_check_interval, key_generation],
    ValidationResults = [{K, validate_value(K, Opts, Defaults)} || K <- CacheOpts],
    ErrorList = lists:dropwhile(
            fun ({K, {invalid, K}}) -> false; ({_, _}) -> true end, ValidationResults),
    case ErrorList of
        [] ->
            {ok, arrange_evict_interval(ValidationResults)};
        [{K, {invalid, K}=E} | _] -> {error, E}
    end.

%% @private
-spec arrange_evict_interval(cache_opts()) -> cache_opts().
arrange_evict_interval(CacheOpts) ->
    case proplists:get_value(evict_interval, CacheOpts) of
        default ->
            EvictInterval = proplists:get_value(validity, CacheOpts)
                            + proplists:get_value(evict, CacheOpts),
            lists:keystore(evict_interval, 1, CacheOpts, {evict_interval, EvictInterval});
        _ ->
            CacheOpts
    end.

%% @private
-spec validate_value(config_key(), [cache_opts()], [cache_opts()]) ->
    term() | {invalid, config_key()}.
validate_value(Key, Opts, Defaults) when Key==refresh_callback; Key==is_error_callback ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> default(Key, Defaults);
        {M, F, A} when is_atom(M) andalso is_atom(F) andalso is_list(A) -> {M, F, A};
        Fun when is_function(Fun) -> Fun;
        _ -> {invalid, Key}
    end;
validate_value(Key, Opts, Defaults) when Key==validity; Key==evict_interval;
        Key==error_validity; Key==mem_check_interval ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> default(Key, Defaults);
        N when is_integer(N) andalso N>0 -> N;
        _ -> {invalid, Key}
    end;
validate_value(Key, Opts, Defaults) when Key==evict; Key==max_cache_size ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> default(Key, Defaults);
        N when is_integer(N) andalso N>=0 -> N;
        _ -> {invalid, Key}
    end;
validate_value(Key, Opts, Defaults) when Key==wait_for_refresh; Key==wait_until_done ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> default(Key, Defaults);
        B when is_boolean(B) -> B;
        _ -> {invalid, Key}
    end;
validate_value(Key, Opts, Defaults) when Key==key_generation ->
    case proplists:get_value(Key, Opts, undefined) of
        undefined -> default(Key, Defaults);
        M when is_atom(M) -> M;
        _ -> {invalid, Key}
    end.

%% @private
-spec default(config_key(), cache_opts()) -> term().
default(max_cache_size, Defaults) ->
    proplists:get_value(max_cache_size, Defaults, ?DEFAULT_MAX_CACHE_SIZE);
default(mem_check_interval, Defaults) ->
    proplists:get_value(mem_check_interval, Defaults, ?DEFAULT_MEM_CHECK_INTERVAL);
default(validity, Defaults) ->
    proplists:get_value(validity, Defaults, ?DEFAULT_VALIDITY);
default(error_validity, Defaults) ->
    proplists:get_value(error_validity, Defaults, ?DEFAULT_ERROR_VALIDITY);
default(evict, Defaults) ->
    proplists:get_value(evict, Defaults, ?DEFAULT_EVICT);
default(evict_interval, Defaults) ->
    proplists:get_value(evict_interval, Defaults, default);
default(wait_for_refresh, Defaults) ->
    proplists:get_value(wait_for_refresh, Defaults, ?DEFAULT_WAIT_FOR_REFRESH);
default(refresh_callback, Defaults) ->
    proplists:get_value(refresh_callback, Defaults, ?DEFAULT_REFRESH_CALLBACK);
default(is_error_callback, Defaults) ->
    proplists:get_value(is_error_callback, Defaults, fun is_error/1);
default(wait_until_done, Defaults) ->
    proplists:get_value(wait_until_done, Defaults, ?DEFAULT_WAIT_UNTIL_DONE);
default(key_generation, Defaults) ->
    proplists:get_value(key_generation, Defaults, undefined);
default(_, _) -> undefined.

%% @private
-spec is_error(value()) -> boolean().
is_error(error) -> true;
is_error({error, _}) -> true;
is_error(_) -> false.

%% @private
-spec is_available_name(name()) -> boolean().
is_available_name(Name) ->
    ets:lookup(?CACHE_MAP, Name)==[] andalso erlang:whereis(Name)==undefined
        andalso erl_cache_server:is_valid_name(Name).

%% @private
-spec is_cache_server(name()) -> boolean().
is_cache_server(Name) ->
    ets:lookup(?CACHE_MAP, Name)/=[].

%% @private
-spec get_name_defaults(name()) -> cache_opts() | undefined.
get_name_defaults(Name) ->
    case ets:lookup(?CACHE_MAP, Name) of
        [{Name, Opts}] -> Opts;
        [] -> undefined
    end.

