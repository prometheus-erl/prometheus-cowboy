-module(prometheus_cowboy1_handler_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% MACROS
%% ===================================================================

-define(PROMETHEUS_ACCEPT,
    "application/vnd.google.protobuf;"
    "proto=io.prometheus.client.MetricFamily;encoding=delimited;q=0.7,"
    "text/plain;version=0.0.4;q=0.3,"
    "application/json;schema=\"prometheus/telemetry\";version=0.0.2;q=0.2,"
    "*/*;q=0.1"
).

-define(TELEMETRY_METRICS_METADATA, [
    "# TYPE telemetry_scrape_duration_seconds summary",
    "# HELP telemetry_scrape_duration_seconds Scrape duration",
    "# TYPE telemetry_scrape_size_bytes summary",
    "# HELP telemetry_scrape_size_bytes Scrape size, not encoded",
    "# TYPE telemetry_scrape_encoded_size_bytes summary",
    "# HELP telemetry_scrape_encoded_size_bytes Scrape size, encoded"
]).

-define(METRICS_URL,
    "http://localhost:" ++ integer_to_list(?config(port, Config)) ++ "/metrics"
).

-define(METRICS_URL(Registry),
    ?METRICS_URL ++ "/" ++ Registry
).

-define(AUTH_TESTS,
    {ok, DeniedR1} =
        httpc:request(get, {?METRICS_URL, []}, [], []),
    ?assertMatch(403, status(DeniedR1)),
    {ok, DeniedR2} =
        httpc:request(
            get,
            {?METRICS_URL, [{"Authorization", "Basic cXdlOnF3ZQ=="}]},
            [],
            []
        ),
    ?assertMatch(403, status(DeniedR2)),
    {ok, DeniedR3} =
        httpc:request(
            get,
            {?METRICS_URL, [{"Authorization", "Basic abba"}]},
            [],
            []
        ),
    ?assertMatch(403, status(DeniedR3)),
    {ok, DeniedR4} =
        httpc:request(
            get,
            {?METRICS_URL, [{"Authorization", "Bearer abba"}]},
            [],
            []
        ),
    ?assertMatch(403, status(DeniedR4)),
    {ok, BasicLPR} =
        httpc:request(
            get,
            {?METRICS_URL, [{"Authorization", "Basic cXdlOnF3YQ=="}]},
            [],
            []
        ),
    ?assertMatch(200, status(BasicLPR))
).

%% @doc All tests of this suite.
all() ->
    [
        {group, positive}
    ].

%% @doc Groups of tests
groups() ->
    [
        {positive, [sequential], [
            prometheus_cowboy1_negotiation,
            prometheus_cowboy1_negotiation_fail,

            prometheus_cowboy1,

            prometheus_cowboy1_registry,
            prometheus_cowboy1_registry_conflict,

            prometheus_cowboy1_auth_basic1,
            prometheus_cowboy1_auth_basic2,
            prometheus_cowboy1_auth_basic3,

            prometheus_cowboy1_auth_provider1,
            prometheus_cowboy1_auth_provider2,

            prometheus_cowboy1_auth_invalid
        ]}
    ].

%% @doc Start the application.
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(prometheus),
    Port = prometheus_cowboy1_app:start(),
    %% debugger:start(),
    %% timer:sleep(80000),
    [{port, Port} | Config].

%% @doc Stop the application.
end_per_suite(Config) ->
    ok = application:stop(cowboy),
    ok = application:stop(prometheus),
    Config.

end_per_testcase(_, Config) ->
    application:set_env(prometheus, prometheus_http, []),
    Config.

%% ===================================================================
%% TESTS
%% ===================================================================

prometheus_cowboy1_negotiation(Config) ->
    {ok, TextResponse} =
        httpc:request(get, {?METRICS_URL, [{"Accept-Encoding", "deflate"}]}, [], []),
    ?assertMatch(200, status(TextResponse)),
    TextCT = prometheus_text_format:content_type(),
    ExpectedTextCT = binary_to_list(TextCT),
    ?assertMatch(
        [
            {"content-encoding", "deflate"},
            {"content-length", ExpectedTextCL},
            {"content-type", ExpectedTextCT}
            | _
        ] when
            ExpectedTextCL > 0,
        headers(TextResponse)
    ),
    ?assert(iolist_size(body(TextResponse)) > 0),

    {ok, ProtobufResponse} =
        httpc:request(
            get,
            {?METRICS_URL, [
                {"Accept", ?PROMETHEUS_ACCEPT},
                {"Accept-Encoding", "gzip, sdch"}
            ]},
            [],
            []
        ),
    ?assertMatch(200, status(ProtobufResponse)),
    ProtobufCT = prometheus_protobuf_format:content_type(),
    ExpectedProtobufCT = binary_to_list(ProtobufCT),
    ?assertMatch(
        [
            {"content-encoding", "gzip"},
            {"content-length", ExpectedProtobufCL},
            {"content-type", ExpectedProtobufCT}
            | _
        ] when
            ExpectedProtobufCL > 0,
        headers(ProtobufResponse)
    ),
    ?assert(iolist_size(zlib:gunzip(body(ProtobufResponse))) > 0),

    application:set_env(
        prometheus,
        prometheus_http,
        [{format, prometheus_protobuf_format}]
    ),
    {ok, ProtobufResponse1} =
        httpc:request(get, {?METRICS_URL, []}, [], []),
    ?assertMatch(200, status(ProtobufResponse1)),
    ProtobufCT = prometheus_protobuf_format:content_type(),
    ExpectedProtobufCT = binary_to_list(ProtobufCT),
    ?assertMatch(
        [
            {"content-encoding", "identity"},
            {"content-length", ExpectedProtobufCL1},
            {"content-type", ExpectedProtobufCT}
            | _
        ] when
            ExpectedProtobufCL1 > 0,
        headers(ProtobufResponse1)
    ),
    ?assert(iolist_size(body(ProtobufResponse)) > 0).

prometheus_cowboy1_negotiation_fail(Config) ->
    {ok, IdentityResponse} =
        httpc:request(get, {?METRICS_URL, [{"Accept-Encoding", "qwe"}]}, [], []),
    ?assertMatch(200, status(IdentityResponse)),
    IdentityCT = prometheus_text_format:content_type(),
    ExpectedIdentityCT = binary_to_list(IdentityCT),
    ?assertMatch(
        [
            {"content-encoding", "identity"},
            {"content-length", ExpectedIdentityCL},
            {"content-type", ExpectedIdentityCT}
            | _
        ] when
            ExpectedIdentityCL > 0,
        headers(IdentityResponse)
    ),

    {ok, FEResponse} =
        httpc:request(get, {?METRICS_URL, [{"Accept-Encoding", "qwe, *;q=0"}]}, [], []),
    ?assertMatch(406, status(FEResponse)),
    ?assertMatch([{"content-length", "0"} | _], headers(FEResponse)),

    {ok, CTResponse} =
        httpc:request(get, {?METRICS_URL, [{"Accept", "image/png"}]}, [], []),
    ?assertMatch(406, status(CTResponse)),
    ?assertMatch(
        [{"content-length", "0"} | _],
        headers(CTResponse)
    ).

prometheus_cowboy1(Config) ->
    {ok, MetricsResponse} = httpc:request(?METRICS_URL),
    ?assertMatch(200, status(MetricsResponse)),
    MetricsCT = prometheus_text_format:content_type(),
    ExpecteMetricsCT = binary_to_list(MetricsCT),
    ?assertMatch(
        [
            {"content-encoding", "identity"},
            {"content-length", ExpectedMetricsCL},
            {"content-type", ExpecteMetricsCT}
            | _
        ] when
            ExpectedMetricsCL > 0,
        headers(MetricsResponse)
    ),
    MetricsBody = body(MetricsResponse),
    ?assertMatch(true, all_telemetry_metrics_present(MetricsBody)),

    {ok, CTResponse} =
        httpc:request(get, {?METRICS_URL("qwe"), []}, [], []),
    ?assertMatch(404, status(CTResponse)),
    ?assertMatch(
        [{"content-length", CL404} | _] when
            CL404 > 0,
        headers(CTResponse)
    ).

prometheus_cowboy1_registry(Config) ->
    prometheus_counter:new([{registry, qwe}, {name, qwe}, {help, ""}]),
    prometheus_counter:inc(qwe, qwe, [], 10),

    {ok, MetricsResponse} = httpc:request(?METRICS_URL("qwe")),
    ?assertMatch(200, status(MetricsResponse)),
    MetricsCT = prometheus_text_format:content_type(),
    ExpecteMetricsCT = binary_to_list(MetricsCT),
    ?assertMatch(
        [
            {"content-encoding", "identity"},
            {"content-length", ExpectedMetricsCL},
            {"content-type", ExpecteMetricsCT}
            | _
        ] when
            ExpectedMetricsCL > 0,
        headers(MetricsResponse)
    ),
    MetricsBody = body(MetricsResponse),
    ?assertMatch(false, all_telemetry_metrics_present(MetricsBody)),
    ?assertMatch({match, _}, re:run(MetricsBody, "# TYPE qwe counter")),

    {ok, IRResponse} =
        httpc:request(get, {?METRICS_URL("qwa"), []}, [], []),
    ?assertMatch(404, status(IRResponse)),
    ?assertMatch(
        [{"content-length", CL404} | _] when
            CL404 > 0,
        headers(IRResponse)
    ).

prometheus_cowboy1_registry_conflict(Config) ->
    application:set_env(
        prometheus,
        prometheus_http,
        [{registry, default}]
    ),

    {ok, DeniedR1} =
        httpc:request(get, {?METRICS_URL("qwe"), []}, [], []),
    ?assertMatch(409, status(DeniedR1)).

prometheus_cowboy1_auth_basic1(Config) ->
    application:set_env(prometheus, prometheus_http, [{authorization, {basic, "qwe", "qwa"}}]),

    ?AUTH_TESTS.

prometheus_cowboy1_auth_basic2(Config) ->
    application:set_env(prometheus, prometheus_http, [{authorization, {basic, ?MODULE}}]),

    ?AUTH_TESTS.

prometheus_cowboy1_auth_basic3(Config) ->
    application:set_env(
        prometheus,
        prometheus_http,
        [{authorization, {basic, {?MODULE, authorize}}}]
    ),

    ?AUTH_TESTS.

prometheus_cowboy1_auth_provider1(Config) ->
    application:set_env(
        prometheus,
        prometheus_http,
        [{authorization, {?MODULE, authorize}}]
    ),

    ?AUTH_TESTS.

prometheus_cowboy1_auth_provider2(Config) ->
    application:set_env(
        prometheus,
        prometheus_http,
        [{authorization, ?MODULE}]
    ),

    ?AUTH_TESTS.

prometheus_cowboy1_auth_invalid(Config) ->
    application:set_env(
        prometheus,
        prometheus_http,
        [{authorization, "qwe"}]
    ),

    {ok, DeniedR1} =
        httpc:request(get, {?METRICS_URL, []}, [], []),
    ?assertMatch(500, status(DeniedR1)).

authorize("qwe", "qwa") ->
    true;
authorize(_, _) ->
    false.

authorize(#{headers := Headers}) ->
    case Headers("authorization", undefined) of
        undefined ->
            false;
        "Basic cXdlOnF3ZQ==" ->
            false;
        "Basic abba" ->
            false;
        "Bearer abba" ->
            false;
        <<"Basic cXdlOnF3ZQ==">> ->
            false;
        <<"Basic abba">> ->
            false;
        <<"Bearer abba">> ->
            false;
        _ ->
            true
    end.

%% ===================================================================
%% Private parts
%% ===================================================================

all_telemetry_metrics_present(Body) ->
    lists:all(
        fun(Metric) ->
            case re:run(Body, Metric) of
                {match, _} -> true;
                _ -> false
            end
        end,
        ?TELEMETRY_METRICS_METADATA
    ).

%%% Helpers

status({{_, Status, _}, _, _}) ->
    Status.
body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).
