-module(leonwly_tcp_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).

start() ->
    application:start(leonwly_tcp_server).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    leonwly_tcp_server_sup:start_link().

stop(_State) ->
    ok.
