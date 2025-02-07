-module(prometheus_cowboy).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("Application module").

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

-export([to_cowboy_headers/1]).

?DOC("Start application").
-spec start(application:start_type(), term()) -> supervisor:startlink_ret().
start(_, _) ->
    case code:ensure_loaded(cowboy_metrics_h) of
        {module, _} -> prometheus_cowboy2_instrumenter:setup();
        _ -> ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

?DOC("Stop application").
-spec stop(term()) -> ok.
stop(_) -> ok.

?DOC(false).
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.

%% ===================================================================
%% API
%% ===================================================================

?DOC(false).
-spec to_cowboy_headers([{atom(), binary()}]) -> [{binary(), binary()}].
to_cowboy_headers(RespHeaders) ->
    lists:map(fun to_cowboy_headers_/1, RespHeaders).

%% ===================================================================
%% Private functions
%% ===================================================================

to_cowboy_headers_({Name, Value}) ->
    {to_cowboy_name(Name), Value}.

to_cowboy_name(Name) ->
    binary:replace(atom_to_binary(Name, utf8), <<"_">>, <<"-">>).
