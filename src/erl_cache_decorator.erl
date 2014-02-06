-module(erl_cache_decorator).

-export([cache_pt/3]).

%% ====================================================================
%% API
%% ====================================================================

-spec cache_pt(function(), [term()], {atom(), atom(), erl_cache:name(), erl_cache:cache_opts()}) ->
    (fun(() -> term())).
cache_pt(Fun, Args, {Module, FunctionAtom, Name, Opts}) ->
    FinalOpts = [{refresh_callback, fun () -> Fun(Args) end} | Opts],
    Key = {decorated, Module, FunctionAtom, erlang:phash2(Args)},
    FromCache = erl_cache:get(Name, Key, FinalOpts),
    case FromCache of
        {ok, Result} -> fun() -> Result end;
        {error, not_found} ->
            fun () ->
                    Res = Fun(Args),
                    ok = erl_cache:set(Name, Key, Res, FinalOpts),
                    Res
            end;
        {error, Err} ->
            throw({error, {cache_pt, Err}})
    end.

