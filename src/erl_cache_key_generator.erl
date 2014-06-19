-module(erl_cache_key_generator).

%% FIXME The Function name will not match the exact function name for the call
%% that is being cached. The reason for that is that when using the parse
%% transform the FunctionAtom that the erl_cache_decorator sees is the one of
%% the internal represention around which the caching behavior has been added.
%%
%% The way to fix this issue is modifying how erl_decorator_pt deals with the
%% intermediate code representation.
-callback generate_key(CacheInstance::erl_cache:name(), Module::atom(),
                       Function::atom(), Args::[term()]) -> erl_cache:key().
