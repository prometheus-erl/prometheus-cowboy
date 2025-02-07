-module(prometheus_cowboy2_instrumenter_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% MACROS
%% ===================================================================

-define(QWE_URL,
    "http://localhost:" ++ integer_to_list(?config(port, Config)) ++ "/qwe"
).

all() ->
    [
        {group, positive}
    ].

%% @doc Groups of tests
groups() ->
    [
        {positive, [sequential], [
            metrics_setup,
            prometheus_cowboy2_instrumenter,
            custom_labels_module,
            custom_registry,
            real_handler
        ]}
    ].

%% @doc Start the application.
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(prometheus),
    cowboy:stop_listener(http),
    {Port, Listener} = prometheus_cowboy2_app:start(),
    [{port, Port}, {listener, Listener} | Config].

%% @doc Stop the application.
end_per_suite(Config) ->
    ok = application:stop(cowboy),
    ok = application:stop(prometheus),
    Config.

init_per_testcase(metrics_setup, Config) ->
    application:set_env(prometheus, cowboy_instrumenter, [
        {early_error_labels, [method]},
        {request_labels, [method]},
        {error_labels, [method]},
        {duration_buckets, [0, 1]}
    ]),
    init_per_testcase(qqq, Config);
init_per_testcase(custom_labels_module, Config) ->
    application:set_env(prometheus, cowboy_instrumenter, [
        {labels_module, ?MODULE},
        {early_error_labels, [qwe]}
    ]),
    init_per_testcase(qqq, Config);
init_per_testcase(custom_registry, Config) ->
    application:set_env(prometheus, cowboy_instrumenter, [{registry, qwe}]),
    init_per_testcase(qqq, Config);
init_per_testcase(_, Config) ->
    prometheus_cowboy2_instrumenter:setup(),
    Config.

end_per_testcase(_, Config) ->
    prometheus_registry:clear(),
    application:unset_env(prometheus, cowboy_instrumenter),
    Config.

label_value(qwe, _Metrics) ->
    "qwe";
label_value(_, _) ->
    undefined.

%% ===================================================================
%% TESTS
%% ===================================================================

metrics_setup(_Config) ->
    ?assertMatch(
        false,
        prometheus_counter:declare([
            {name, cowboy_early_errors_total},
            {labels, [method]},
            {help, ""}
        ])
    ),
    ?assertMatch(
        false,
        prometheus_counter:declare([
            {name, cowboy_protocol_upgrades_total},
            {labels, [method]},
            {help, ""}
        ])
    ),
    %% each observe call means new request
    ?assertMatch(
        false,
        prometheus_counter:declare([
            {name, cowboy_requests_total},
            {labels, [method]},
            {help, ""}
        ])
    ),
    ?assertMatch(
        false,
        prometheus_counter:declare([
            {name, cowboy_spawned_processes_total},
            {labels, [method]},
            {help, ""}
        ])
    ),
    ?assertMatch(
        false,
        prometheus_counter:declare([
            {name, cowboy_errors_total},
            {labels, [method]},
            {help, ""}
        ])
    ),
    ?assertMatch(
        false,
        prometheus_histogram:declare([
            {name, cowboy_request_duration_seconds},
            {labels, [method]},
            {buckets, [0, 1]},
            {help, ""}
        ])
    ),
    ?assertMatch(
        false,
        prometheus_histogram:declare([
            {name, cowboy_receive_body_duration_seconds},
            {labels, [method]},
            {buckets, [0, 1]},
            {help, ""}
        ])
    ).

prometheus_cowboy2_instrumenter(Config) ->
    Listener = ?config(listener, Config),
    prometheus_cowboy2_instrumenter:observe(#{early_error_time => 1, ref => Listener}),
    prometheus_cowboy2_instrumenter:observe(generate_metrics(switch_protocol, 3, Listener)),
    prometheus_cowboy2_instrumenter:observe(generate_metrics(normal, 3, Listener)),
    prometheus_cowboy2_instrumenter:observe(
        generate_metrics({connection_error, protocol_error, ""}, 5, Listener)
    ),

    ?assertMatch(1, prometheus_counter:value(cowboy_early_errors_total)),
    ?assertMatch(1, prometheus_counter:value(cowboy_protocol_upgrades_total)),

    ?assertMatch(
        1, prometheus_counter:value(cowboy_requests_total, [<<"POST">>, normal, "client-error"])
    ),
    ?assertMatch(
        {[0, 0, 0, 0, 0, 1, 0, 0, 0, 0], 1.0},
        prometheus_histogram:value(cowboy_request_duration_seconds, [
            <<"POST">>, normal, "client-error"
        ])
    ),
    ?assertMatch(
        {[0, 1, 0, 0, 0, 0, 0, 0, 0, 0], 0.1},
        prometheus_histogram:value(cowboy_receive_body_duration_seconds, [
            <<"POST">>, normal, "client-error"
        ])
    ),
    ?assertMatch(
        3,
        prometheus_counter:value(cowboy_spawned_processes_total, [
            <<"POST">>, normal, "client-error"
        ])
    ),

    ?assertMatch(
        1,
        prometheus_counter:value(cowboy_requests_total, [
            <<"POST">>, connection_error, "client-error"
        ])
    ),
    ?assertMatch(
        {[0, 0, 0, 0, 0, 1, 0, 0, 0, 0], 1.0},
        prometheus_histogram:value(cowboy_request_duration_seconds, [
            <<"POST">>, connection_error, "client-error"
        ])
    ),
    ?assertMatch(
        {[0, 1, 0, 0, 0, 0, 0, 0, 0, 0], 0.1},
        prometheus_histogram:value(cowboy_receive_body_duration_seconds, [
            <<"POST">>, connection_error, "client-error"
        ])
    ),
    ?assertMatch(
        5,
        prometheus_counter:value(cowboy_spawned_processes_total, [
            <<"POST">>, connection_error, "client-error"
        ])
    ),

    ?assertMatch(
        1,
        prometheus_counter:value(cowboy_errors_total, [
            <<"POST">>, connection_error, protocol_error
        ])
    ),

    ok.

custom_labels_module(Config) ->
    Listener = ?config(listener, Config),
    prometheus_cowboy2_instrumenter:observe(#{early_error_time => 1, ref => Listener}),
    ?assertMatch(1, prometheus_counter:value(cowboy_early_errors_total, ["qwe"])).

custom_registry(Config) ->
    Listener = ?config(listener, Config),
    prometheus_cowboy2_instrumenter:observe(#{early_error_time => 1, ref => Listener}),
    ?assertMatch(1, prometheus_counter:value(qwe, cowboy_early_errors_total, [])).

real_handler(Config) ->
    {ok, _} =
        httpc:request(get, {?QWE_URL, [{"Accept-Encoding", "deflate"}]}, [], []),
    ?assertMatch(
        1, prometheus_counter:value(cowboy_requests_total, [<<"GET">>, normal, "success"])
    ).

%% ===================================================================
%% Private functions
%% ===================================================================

generate_metrics(Reason, Procs, Listener) ->
    #{
        ref => Listener,
        req_start => 1,
        req_end => 1000000001,
        req_body_start => 100000000,
        req_body_end => 200000000,
        reason => Reason,
        req => #{method => <<"POST">>},
        resp_status => 401,
        procs => generate_procs(Procs)
    }.

generate_procs(Procs) ->
    maps:from_list([{N, info} || N <- lists:seq(1, Procs)]).
