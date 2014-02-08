<!---
@doc <!-- -->

<h3> Purpose </h3>

This application is meant to facilitate the process of caching function calls within an erlang node.
Do not expect a complex distributed application here. There are far more complex products out there
intended to act that way. Istead, erl_cache intends to be a simple solution that unifies common
caching patterns within an erlang node which you probably have implemented a thousand times in one
thousand slightly different ways.

<h3> Functional Description </h3>

The erl_cache module acts as the main application interface, allowing you to start/stop independent
cache servers and interact with them.

Each entry in a cache can be either valid, stale or evictable. The difference between stale and
evictable is that, when a stale entry is requested, and in case a refresh callback was indicated
when first setting it, it will be refreshed and so back into valid state. Evictable entries will be
removed from cache when hit and never returned to the client.

From a user point of view, those independent cache servers provide independent namespacing. Each
cache server uses its own set of default options and can crash without affecting any of the others.

From a system point of view, erl_cache acts as a server of caches, holding the cache names and their
associated defaults in a protected ets table. erl_cache is also responsible for option validation in
every call.

<img src="doc/images/erl_cache.png" style="max-height: 500px;"/>

erl_cache_server holds the actual cache in its own protected ets table and implements the logic
behind the refreshing stale entries when hit and the eviction of old entries

<h3> Configuration </h3>

This application accepts only one configuration option: cache_servers. This is a list of 2-tuples
indicating the name and the default options for each one of the caches the application should bring
up at startup. The format is the same used in <code>erl_cache:start_cache/2</code>. i.e.

<code>
[{erl_cache, [
    {cache_servers, [{my_cache_server, [{wait_until_done, true}, {validity, 5000}, {evict, 3000}]}]}
]}].
</code>

The default config options unless otherwise specified are:

<li>wait_for_refresh: true</li>
<li>wait_until_done: false</li>
<li>validity: 300000</li>
<li>evict: 60000</li>
<li>evict_interval: ServerLevelEvict + ServerLevelValidity</li>
<li>refresh_callback: undefined</li>

Since the evict_interval option can only be applied at cache server start up time, it
should be set carefully. This option controls how often entries to be evicted will be deleted from
the cache. There is only one global evict_interval per cache_server and specific validity and evict
values passed to set operations or to the ?CACHE macro will not affect it. The evict_interval option
will be ignored in all function calls except for <code>erl_cache:start_server/2</code>.

<h3> The ?CACHE macro </h3>

For ease of use, this application provides the <code>?CACHE</code> macro. This macro can be placed on top of
any public function. Every time the function is invoked, erl_cache will try to retrieve the suitable
return value from cache and, in case it's not there, perform the regular function call and cache the
result. Here you can see an example of how to use the macro to avoid sums being performed everytime
sum/2 is called:

<code>
?CACHE(my_cache_namespace, [{validity, 10000}, {evict, 2000}}]).
sum(A, B) ->
    A + B.
</code>

