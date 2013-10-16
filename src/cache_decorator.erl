-module(cache_decorator).

-export([
		cache_pt/3
	]).

%% ====================================================================
%% API
%% ====================================================================

-spec cache_pt(function(), [term()], {atom(), atom(), cache_facade:cache_options()}) ->
	function().
cache_pt(Fun, Args, {Module, FunctionAtom, Options}) ->
	NewOptions = [
		{ttl, proplists:get_value(ttl, Options, default)},
		{evict, proplists:get_value(evict, Options, default)},
		{refresh_function, Fun},
		{refresh_args, Args},
		{refresh_miss, proplists:get_value(refresh_miss, Options, synchronous)},
		{refresh_stale, proplists:get_value(refresh_stale, Options, asynchronous)},
		{refresh_evict, proplists:get_value(refresh_evict, Options, synchronous)}
	],
	Key = {decorated, Module, FunctionAtom, Args},
	case cache_facade:get(Key, NewOptions) of
		{ok, Result} ->
			fun() -> Result end;
		{error, Err} ->
			throw({error, {cache, Err}})
	end.
