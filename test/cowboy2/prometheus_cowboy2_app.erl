%% @private
-module(prometheus_cowboy2_app).

%% API.
-export([start/0]).
%% API.

start() ->
    prometheus_http_impl:setup(),

    Routes = [
        {'_', [
            {"/metrics/[:registry]", prometheus_cowboy2_handler, []},
            {"/qwe", qwe_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(
        http,
        [{port, 0}],
        #{
            env => #{dispatch => Dispatch},
            metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
            stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
        }
    ),
    {ranch:get_port(http), http}.
