-module(prometheus_cowboy2_handler).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-endif.
?MODULEDOC(false).

%% -behaviour(cowboy_handler).

-export([init/2, terminate/3]).

%% ===================================================================
%% cowboy_handler callbacks
%% ===================================================================

-spec init(cowboy_req:req(), cowboy_http:opts()) ->
    {ok, cowboy_req:req(), undefined}.
init(Req, _Opts) ->
    handle(Req).

-spec terminate(term(), cowboy_req:req(), undefined) ->
    ok.
terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Private functions
%% ===================================================================

handle(Request) ->
    Method = cowboy_req:method(Request),
    Request1 = gen_response(Method, Request),
    {ok, Request1, undefined}.

gen_response(<<"GET">>, Request) ->
    Registry0 = cowboy_req:binding(registry, Request, <<"default">>),
    case prometheus_registry:exists(Registry0) of
        false ->
            cowboy_req:reply(404, #{}, <<"Unknown Registry">>, Request);
        Registry ->
            gen_metrics_response(Registry, Request)
    end;
gen_response(_, Request) ->
    Request.

-dialyzer({nowarn_function, [gen_metrics_response/2]}).
gen_metrics_response(Registry, Request) ->
    URI = true,
    GetHeader = fun(Name, Default) ->
        cowboy_req:header(
            iolist_to_binary(Name),
            Request,
            Default
        )
    end,
    {Code, RespHeaders, Body} = prometheus_http_impl:reply(
        #{
            path => URI,
            headers => GetHeader,
            registry => Registry,
            standalone => false
        }
    ),

    Headers = prometheus_cowboy:to_cowboy_headers(RespHeaders),
    cowboy_req:reply(Code, maps:from_list(Headers), Body, Request).
