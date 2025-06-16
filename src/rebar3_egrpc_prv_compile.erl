-module(rebar3_egrpc_prv_compile).

-export([init/1, do/1, format_error/1]).

-include_lib("providers/include/providers.hrl").

-define(PROVIDER, gen).
-define(NAMESPACE, egrpc).
-define(DEPS, [{default, app_discovery}, {protobuf, compile}]).
-define(DEFAULT_TEMPLATE_FILE, "egrpc_service.erl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name,       ?PROVIDER},                     % The 'user friendly' name of the task
        {namespace,  ?NAMESPACE},
        {module,     ?MODULE},                       % The module implementation of the task
        {bare,       true},                          % The task can be run by the user, always true
        {deps,       ?DEPS},                         % The list of dependencies
        {example,    "rebar3 egrpc gen"},             % How to use the plugin
        {opts,       []},                            % list of options understood by the plugin
        {short_desc, "Generates gRPC client protos"},
        {desc,       "Generates gRPC client protos"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Config = rebar_state:opts(State),
    EgrpcOpts = rebar_opts:get(Config, egrpc_opts, []),
    {Options, _} = rebar_state:command_parsed_args(State),
    ProtoDir = proplists:get_value(protos, Options, proplists:get_value(protos, EgrpcOpts, "priv/protos")),
    GpbOpts = rebar_opts:get(Config, gpb_opts, []),
    rebar_api:debug("egrpc will use gpb options: ~p", [GpbOpts]),

    %% TODO: Keep this the same discover logic as in rebar3_gpb_compiler
    ProtoFiles0 = filelib:wildcard([ProtoDir, "/**/*.proto"]),
    ProtoFiles = case proplists:get_value(f, GpbOpts, undefined) of
        undefined ->
            ProtoFiles0;
        WantedProtos ->
            lists:filter(fun(F) ->
                                 lists:member(filename:basename(F), WantedProtos)
                         end, ProtoFiles0)
    end,
    rebar_api:debug("egrpc will compile these proto files: ~p", [ProtoFiles]),

    [gen_client_module(ProtoFilename, Options, GpbOpts, EgrpcOpts, State) || ProtoFilename <- ProtoFiles],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

find_pb_file(ProtoFilename, GpbOpts) ->
    OutDir = proplists:get_value(o, GpbOpts, "src/protos"),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts, "_pb"),
    ModuleNamePrefix = proplists:get_value(module_name_prefix, GpbOpts, ""),
    PbFilename = filename:join(OutDir, ModuleNamePrefix ++ filename:basename(ProtoFilename, ".proto") ++ ModuleNameSuffix ++ ".erl"),
    case file:read_file_info(PbFilename) of
        {ok, _} ->
            rebar_api:info("Found existing pb file ~s", [PbFilename]),
            PbFilename;
        {error, enoent} ->
            rebar_api:error("No existing pb file found at ~s", [PbFilename]),
            throw(?PRV_ERROR({no_pb_file, PbFilename}))
    end.

compile_pb(PbFilename) ->
    GpbIncludeDir = filename:join(code:lib_dir(gpb), "include"),
    case compile:file(PbFilename,
        [binary, {i, GpbIncludeDir}, {i, "./include/"}, return_errors]) of
        {ok, Module, Compiled} ->
            {module, _} = code:load_binary(Module, PbFilename, Compiled),
            Module;
        {ok, Module, Compiled, Warnings} ->
            [begin
                 rebar_api:warn("Warning building ~s", [File]),
                 [rebar_api:warn("        ~p: ~s", [Line, M:format_error(E)]) || {Line, M, E} <- Es]
             end || {File, Es} <- Warnings],
            {module, _} = code:load_binary(Module, PbFilename, Compiled),
            Module;
        {error, Errors, Warnings} ->
            throw(?PRV_ERROR({compile_errors, Errors, Warnings}))
    end.

gen_client_module(ProtoFilename, Options, GpbOpts, EgrpcOpts, State) ->
    PbFilename = find_pb_file(ProtoFilename, GpbOpts),
    GpbModule = compile_pb(PbFilename),
    rebar_api:info("Compiled PB file ~s", [PbFilename]),
    OutDir0 = proplists:get_value(out_dir, EgrpcOpts, "src"),
    OutDir = string:trim(OutDir0, trailing, "/"),
    Force = proplists:get_value(force, Options, true),
    Services = [format_services(GpbModule, Service, OutDir, EgrpcOpts) || Service <- GpbModule:get_service_names()],
    rebar_api:debug("egrpc will run client module gen for these services: ~p", [Services]),
    [rebar_templater:new("egrpc_client_service", Service, Force, State) || Service <- Services],
    rebar_api:info("Generated a client module for PB module ~s", [GpbModule]).

format_services(GpbModule, Service, OutDir, EgrpcOpts) ->
    DocAttribute = proplists:get_value(doc_attribute, EgrpcOpts, false),
    ModulePrefix = proplists:get_value(module_name_prefix, EgrpcOpts, "grpc_"),
    ModuleSuffix = proplists:get_value(module_name_suffix, EgrpcOpts, "_client"),
    {{service, FqServiceName}, RpcDef} = GpbModule:get_service_def(Service),
    rebar_api:debug("egrpc service ~p has methods: ~p", [Service, RpcDef]),
    %% Use fully qualified service name as module name
    ModuleName = lists:join("", [
        ModulePrefix,
        snake_case(atom_to_list(FqServiceName)),
        ModuleSuffix
    ]),

    [
        {out_dir,           OutDir},
        {use_doc_attribute, DocAttribute},
        {pb_module,         atom_to_list(GpbModule)},
        {fq_service_name,   atom_to_list(FqServiceName)},
        {module_name,       ModuleName},
        {methods,           [format_rpc_def(M) || M <- RpcDef]}
    ].

grpc_type(false, false) -> unary;
grpc_type(false, true)  -> server_streaming;
grpc_type(true, false)  -> client_streaming;
grpc_type(true, true)   -> bidi_streaming.

format_rpc_def(#{
    input         := Input,
    input_stream  := InputStream,
    name          := FqMethodName,
    output        := Output,
    output_stream := OutputStream
}) ->
     [
         {method,           snake_case(atom_to_list(FqMethodName))},
         {fq_method_name,   atom_to_list(FqMethodName)},
         {input,            Input},
         {output,           Output},
         {input_stream,     InputStream},
         {output_stream,    OutputStream},
         {grpc_method_type, grpc_type(InputStream, OutputStream)}
     ].

snake_case(NameString) ->
    Snaked = lists:foldl(fun(RE, Snaking) ->
        re:replace(Snaking, RE, "\\1_\\2", [{return, list}, global])
                         end, NameString, [
        %% separte consecutive uppercase letters followed by a capitalised word, e.g. HTTPApi => HTTP_Api
        "([A-Z]+)([A-Z][a-z]+)",
        %% or add _ before capitalised words, e.g. fooTestApi => foo_Test_Api
        "([A-Za-z0-9])([A-Z][a-z]+)",
        %% or separete any consecutive digits followed by alphabet, e.g. v33api => v33_api
        % "([0-9]+)([A-Za-z]+)",
        %% uppercase followed by lowercase or digit, e.g. HealthApi => Health_Api
        "([a-z0-9])([A-Z])"]),
    Snaked1 = string:replace(Snaked, ".", "_", all),
    string:to_lower(unicode:characters_to_list(Snaked1)).
