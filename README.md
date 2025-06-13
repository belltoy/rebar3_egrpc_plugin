rebar3_egrpc_plugin
=====

A rebar3 plugin for generating client side gRPC service codes, for use with [egrpc](https://github.com/belltoy/egrpc).

Build
-----

```
rebar3 compile
```

Use
---

Add the plugin to your rebar config:

``` erlang
{deps, [egrpc]}.

{plugins, [rebar3_gbp_plugin, rebar3_egrpc_plugin]}.
```

and then run generation:

```bash
# Generate pb modules
rebar3 protobuf comfile

# Generate gRPC service client codes
rebar3 egrpc gen
```
