-module(prometheus_cowboy2_instrumenter).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
Collects Cowboy metrics using
[metrics stream handler](https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl)

## Exported metrics

- `cowboy_early_errors_total`:
    Total number of Cowboy early errors, i.e. errors that occur before a request is received.
    - Type: counter.
    - Labels: default - `[]`, configured via `early_errors_labels`.
- `cowboy_protocol_upgrades_total`:
    Total number of protocol upgrades, i.e. when http connection upgraded to websocket connection.
    - Type: counter.
    - Labels: default - `[]`, configured via `protocol_upgrades_labels`.
- `cowboy_requests_total`:
    Total number of Cowboy requests.
    - Type: counter.
    - Labels: default - `[method, reason, status_class]`, configured via `request_labels`.
- `cowboy_spawned_processes_total`:
    Total number of spawned processes.
    - Type: counter.
    - Labels: default - `[method, reason, status_class]`, configured via `request_labels`.
- `cowboy_errors_total`:
    Total number of Cowboy request errors.
    - Type: counter.
    - Labels: default - `[method, reason, error]`, configured via `error_labels`.
- `cowboy_request_duration_seconds`:
    Cowboy request duration.
    - Type: histogram.
    - Labels: default - `[method, reason, status_class]`, configured via `request_labels`.
    - Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]`,
        configured via `duration_buckets`.
- `cowboy_receive_body_duration_seconds`:
    Request body receiving duration.
    - Type: histogram.
    - Labels: default - `[method, reason, status_class]`, configured via `request_labels`.
    - Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]`,
        configured via `duration_buckets`.
- `cowboy_request_body_size_bytes`:
    Request body size in bytes.
    - Type: histogram.
    - Labels: default - `[method, reason, status_class]`, configured via `request_labels`.
    - Buckets: default - `[0 | prometheus_buckets:exponential(1, 8.0, 9)]` (0 KiB - 16 MiB),
        configured via `body_size_buckets`.
- `cowboy_response_body_size_bytes`:
    Response body size in bytes.
    - Type: histogram.
    - Labels: default - `[method, reason, status_class]`, configured via `request_labels`.
    - Buckets: default - `[0 | prometheus_buckets:exponential(1, 8.0, 9)]` (0 KiB - 16 MiB),
        configured via `body_size_buckets`.

## Configuration

Prometheus Cowboy2 instrumenter configured via `cowboy_instrumenter`
key of `prometheus` app environment.

Default configuration:

```
{prometheus, [
    ...
    {cowboy_instrumenter, [{duration_buckets, [0.01, 0.1, 0.25, 0.5,
                                               0.75, 1, 1.5, 2, 4]},
                           {early_error_labels,  []},
                           {request_labels, [method, reason, status_class]},
                           {error_labels, [method, reason, error]},
                           {registry, default}]}
    ...
]}
```

## Labels

Builtin:
 - host,
 - port,
 - method,
 - status,
 - status_class,
 - reason,
 - error.

### Custom labels
Can be implemented via module exporting label_value/2 function.
First argument will be label name, second is Metrics data from
[metrics stream handler](https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl)
Set this module to `labels_module` configuration option.
""").

-export([setup/0]).
-export([observe/1]).

-compile({inline, [inc/2, inc/3, observe/3]}).

-define(DEFAULT_DURATION_BUCKETS, [0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]).
-define(DEFAULT_EARLY_ERROR_LABELS, []).
-define(DEFAULT_PROTOCOL_UPGRADE_LABELS, []).
-define(DEFAULT_REQUEST_LABELS, [method, reason, status_class]).
-define(DEFAULT_ERROR_LABELS, [method, reason, error]).
-define(DEFAULT_LABELS_MODULE, undefined).
-define(DEFAULT_REGISTRY, default).
-define(DEFAULT_CONFIG, [
    {duration_buckets, ?DEFAULT_DURATION_BUCKETS},
    {early_error_labels, ?DEFAULT_EARLY_ERROR_LABELS},
    {protocol_upgrade_labels, ?DEFAULT_PROTOCOL_UPGRADE_LABELS},
    {request_labels, ?DEFAULT_REQUEST_LABELS},
    {error_labels, ?DEFAULT_ERROR_LABELS},
    {lables_module, ?DEFAULT_LABELS_MODULE},
    {registry, ?DEFAULT_REGISTRY}
]).

%% ===================================================================
%% API
%% ===================================================================

?DOC("""
[metrics stream handler](https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl)
callback
""").
-spec observe(map()) -> ok.
observe(#{ref := ListenerRef} = Metrics0) ->
    {Host, Port} = ranch:get_addr(ListenerRef),
    dispatch_metrics(Metrics0#{
        listener_host => Host,
        listener_port => Port
    }),
    ok.

?DOC("Sets all metrics up. Call this when the app starts.").
-spec setup() -> ok.
setup() ->
    prometheus_counter:declare([
        {name, cowboy_early_errors_total},
        {registry, registry()},
        {labels, early_error_labels()},
        {help, "Total number of Cowboy early errors."}
    ]),
    prometheus_counter:declare([
        {name, cowboy_protocol_upgrades_total},
        {registry, registry()},
        {labels, protocol_upgrade_labels()},
        {help, "Total number of protocol upgrades."}
    ]),
    %% each observe call means new request
    prometheus_counter:declare([
        {name, cowboy_requests_total},
        {registry, registry()},
        {labels, request_labels()},
        {help, "Total number of Cowboy requests."}
    ]),
    prometheus_counter:declare([
        {name, cowboy_spawned_processes_total},
        {registry, registry()},
        {labels, request_labels()},
        {help, "Total number of spawned processes."}
    ]),
    prometheus_counter:declare([
        {name, cowboy_errors_total},
        {registry, registry()},
        {labels, error_labels()},
        {help, "Total number of Cowboy request errors."}
    ]),
    prometheus_histogram:declare([
        {name, cowboy_request_duration_seconds},
        {registry, registry()},
        {labels, request_labels()},
        {buckets, duration_buckets()},
        {help, "Cowboy request duration."}
    ]),
    prometheus_histogram:declare([
        {name, cowboy_receive_body_duration_seconds},
        {registry, registry()},
        {labels, request_labels()},
        {buckets, duration_buckets()},
        {help, "Request body receiving duration."}
    ]),
    prometheus_histogram:declare([
        {name, cowboy_request_body_size_bytes},
        {registry, registry()},
        {labels, request_labels()},
        {buckets, body_size_buckets()},
        {help, "Request body size in bytes."}
    ]),
    prometheus_histogram:declare([
        {name, cowboy_response_body_size_bytes},
        {registry, registry()},
        {labels, request_labels()},
        {buckets, body_size_buckets()},
        {help, "Response body size in bytes."}
    ]),
    ok.

%% ===================================================================
%% Private functions
%% ===================================================================

dispatch_metrics(#{early_error_time := _} = Metrics) ->
    inc(cowboy_early_errors_total, early_error_labels(Metrics));
dispatch_metrics(#{reason := switch_protocol} = Metrics) ->
    inc(cowboy_protocol_upgrades_total, protocol_upgrade_labels(Metrics));
dispatch_metrics(
    #{
        req_start := ReqStart,
        req_end := ReqEnd,
        req_body_start := ReqBodyStart,
        req_body_end := ReqBodyEnd,
        req_body_length := ReqBodyLength,
        resp_body_length := RespBodyLength,
        reason := Reason,
        procs := Procs
    } = Metrics
) ->
    RequestLabels = request_labels(Metrics),
    inc(cowboy_requests_total, RequestLabels),
    inc(cowboy_spawned_processes_total, RequestLabels, maps:size(Procs)),
    observe(cowboy_request_duration_seconds, RequestLabels, ReqEnd - ReqStart),
    observe(cowboy_request_body_size_bytes, RequestLabels, ReqBodyLength),
    observe(cowboy_response_body_size_bytes, RequestLabels, RespBodyLength),
    case ReqBodyEnd of
        undefined ->
            ok;
        _ ->
            observe(
                cowboy_receive_body_duration_seconds,
                RequestLabels,
                ReqBodyEnd - ReqBodyStart
            )
    end,

    case Reason of
        normal ->
            ok;
        switch_protocol ->
            ok;
        stop ->
            ok;
        _ ->
            ErrorLabels = error_labels(Metrics),
            inc(cowboy_errors_total, ErrorLabels)
    end.

inc(Name, Labels) ->
    prometheus_counter:inc(registry(), Name, Labels, 1).

inc(Name, Labels, Value) ->
    prometheus_counter:inc(registry(), Name, Labels, Value).

observe(Name, Labels, Value) ->
    prometheus_histogram:observe(registry(), Name, Labels, Value).

%% labels

early_error_labels(Metrics) ->
    compute_labels(early_error_labels(), Metrics).

protocol_upgrade_labels(Metrics) ->
    compute_labels(protocol_upgrade_labels(), Metrics).

request_labels(Metrics) ->
    compute_labels(request_labels(), Metrics).

error_labels(Metrics) ->
    compute_labels(error_labels(), Metrics).

compute_labels(Labels, Metrics) ->
    [label_value(Label, Metrics) || Label <- Labels].

label_value(host, #{listener_host := Host}) ->
    Host;
label_value(port, #{listener_port := Port}) ->
    Port;
label_value(method, #{req := Req}) ->
    cowboy_req:method(Req);
label_value(status, #{resp_status := Status}) ->
    Status;
label_value(status_class, #{resp_status := undefined}) ->
    undefined;
label_value(status_class, #{resp_status := Status}) when is_binary(Status) ->
    undefined;
label_value(status_class, #{resp_status := Status}) when is_integer(Status) ->
    prometheus_http:status_class(Status);
label_value(reason, #{reason := Reason}) ->
    case Reason of
        _ when is_atom(Reason) -> Reason;
        {ReasonAtom, _} -> ReasonAtom;
        {ReasonAtom, _, _} -> ReasonAtom
    end;
label_value(error, #{reason := Reason}) ->
    case Reason of
        _ when is_atom(Reason) -> undefined;
        {_, {Error, _}, _} -> Error;
        {_, Error, _} when is_atom(Error) -> Error;
        _ -> undefined
    end;
label_value(Label, Metrics) ->
    case labels_module() of
        undefined -> undefined;
        Module -> Module:label_value(Label, Metrics)
    end.

%% configuration

config() ->
    application:get_env(prometheus, cowboy_instrumenter, ?DEFAULT_CONFIG).

get_config_value(Key, Default) ->
    proplists:get_value(Key, config(), Default).

duration_buckets() ->
    get_config_value(duration_buckets, ?DEFAULT_DURATION_BUCKETS).

body_size_buckets() ->
    get_config_value(
        body_size_buckets,
        % Range from 0 to 16 MiB
        [0 | prometheus_buckets:exponential(8, 8.0, 8)]
    ).

early_error_labels() ->
    get_config_value(early_error_labels, ?DEFAULT_EARLY_ERROR_LABELS).

protocol_upgrade_labels() ->
    get_config_value(protocol_upgrade_labels, ?DEFAULT_PROTOCOL_UPGRADE_LABELS).

request_labels() ->
    get_config_value(request_labels, ?DEFAULT_REQUEST_LABELS).

error_labels() ->
    get_config_value(error_labels, ?DEFAULT_ERROR_LABELS).

labels_module() ->
    get_config_value(labels_module, ?DEFAULT_LABELS_MODULE).

registry() ->
    get_config_value(registry, ?DEFAULT_REGISTRY).
