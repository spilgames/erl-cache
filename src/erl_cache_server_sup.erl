-module(erl_cache_server_sup).

-behaviour(supervisor).

-include("logging.hrl").

%% API
-export([start_link/0, add_cache/2, remove_cache/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Start the supervisor tree.
-spec start_link() -> {ok, pid()}.
%% @end
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_cache(erl_cache:name(), erl_cache:evict_interval()) -> {ok, pid()}.
add_cache(Name, EvictInterval) ->
    ?INFO("Adding supervised cache '~p'~n", [Name]),
    supervisor:start_child(?MODULE, cache_spec(Name, EvictInterval)).

-spec remove_cache(erl_cache:name()) -> ok.
remove_cache(Name) ->
    ok = supervisor:terminate_child(?MODULE, Name),
    ok = supervisor:delete_child(?MODULE, Name).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc
-spec init(list()) ->
    {ok, {
            {one_for_one, 5, 10},
            [{atom(), {atom(), start_link, [term()]}, permanent, 5000, worker, [atom()]}]
         }
    }.

%% @end
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec cache_spec(erl_cache:name(), erl_cache:evict_interval()) -> supervisor:child_spec().
cache_spec(Name, EvictInterval) ->
    {Name, {erl_cache_server, start_link, [Name, EvictInterval]}, permanent, 5000, worker, [erl_cache_server]}.

