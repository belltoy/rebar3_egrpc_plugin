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

{plugins, [rebar3_gpb_plugin, rebar3_egrpc_plugin]}.
```

and then run generation:

```bash
# Generate pb modules
rebar3 protobuf compile

# Generate gRPC service client codes
rebar3 egrpc gen
```

The plugin reads the already generated pb modules and expects the service definitions as maps, so
`gpb_opts` in your `rebar.config` must contain `maps` (or at least `defs_as_maps`):

``` erlang
{gpb_opts, [
    {i, "priv/protos"},
    {o, "src/protos"},
    maps
]}.
```

The generated `*_pb.erl` files are looked up in the `o` directory of `gpb_opts`.

Options
-------

Configure the generator in `rebar.config`:

``` erlang
{egrpc_opts, [
    {protos, "priv/protos"},
    {out_dir, "src/stubs"},
    {module_name_prefix, "grpc_"},
    {module_name_suffix, "_stub"},
    {doc_attribute, false},
    {client_type, "any()"},
    {method_meta_type, "map()"}
]}.
```

| Option | Default | Description |
|---|---|---|
| `protos` | `"priv/protos"` | Directory to look for `.proto` files |
| `out_dir` | `"src"` | Directory the client modules are written to |
| `module_name_prefix` | `"grpc_"` | Prefix of generated module names |
| `module_name_suffix` | `"_stub"` | Suffix of generated stub module names |
| `doc_attribute` | `false` | Use `-moduledoc`/`-doc` attributes (OTP 27+) instead of edoc comments |
| `client_type` | `"any()"` | Type of the first argument of the generated functions, e.g. `"egrpc:client()"` |
| `method_meta_type` | `"map()"` | Element type of the list returned by `methods/0` |

`client_type` and `method_meta_type` are inserted into the generated specs as-is.

Generated code
--------------

One module is generated for each service. Module names are
`<prefix><snake_cased_fq_service_name><suffix>`.

For each RPC method `Foo`, a pair of functions `foo/N` and `foo/N+1` is generated, the last one
takes an `Opts` argument:

| Method type | Functions |
|---|---|
| unary, server streaming | `foo(Client, Request)`, `foo(Client, Request, Opts)` |
| client streaming, bidi streaming | `foo(Client)`, `foo(Client, Opts)` |

Every module also exports two metadata functions:

``` erlang
service_name() -> 'etcdserverpb.KV'.

methods() ->
    [#{name => 'Range',                      % method name in the proto file
       function => range,                    % generated function name
       path => <<"/etcdserverpb.KV/Range">>,
       grpc_type => unary,                   % unary | server_streaming | client_streaming | bidi_streaming
       input => 'etcdserverpb.RangeRequest',
       output => 'etcdserverpb.RangeResponse',
       idempotency => unknown},              % unknown | no_side_effects | idempotent
     ...].
```

`idempotency` comes from the standard method option `idempotency_level`
(`IDEMPOTENT`, `NO_SIDE_EFFECTS`); it is `unknown` when the option is not set.

Generated files carry no timestamp, so regenerating produces no diff unless the protos or options
changed. Existing files are overwritten.
