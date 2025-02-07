-module(qwe_handler).

%% -behaviour(cowboy_handler).

-export([
    init/2,
    terminate/3
]).

%% ===================================================================
%% cowboy_handler callbacks
%% ===================================================================

init(Req, _Opts) ->
    {ok, cowboy_req:reply(200, #{}, <<"qwe">>, Req), undefined}.

terminate(_Reason, _Req, _State) ->
    ok.
