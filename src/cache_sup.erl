
-module(cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Start the supervisor tree.
-spec start_link() -> {ok, pid()}.
%% @end
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
    {ok, { {one_for_one, 5, 10}, [
    	?CHILD(cache_server, worker, [])
    ]} }.
