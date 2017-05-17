-module(leonwly_tcp_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0]).
-export([
    tcp_listener_started/2,
    tcp_listener_stopped/2,
    start_client/2
]).

-define(APPLICATION, leonwly_tcp_server).

start() ->
    application:start(?APPLICATION).

stop() ->
    application:stop(?APPLICATION).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    leonwly_tcp_server_sup:start_link(),
    case boot_listener_sup() of
        {ok, _Pid} ->
            {ok, self()};
        {error, _Error} ->
            stop()
    end.

stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

boot_listener_sup() ->
    Port = application:get_env(?APPLICATION, tcp_listen_port, 0),
    AcceptorCount = application:get_env(?APPLICATION, tcp_acceptor_count, 1),
    OnStartup = {?MODULE, tcp_listener_started, []},
    OnShutdown = {?MODULE, tcp_listener_stopped, []},
    AcceptCallback = {?MODULE, start_client, []},
    todo.

tcp_listener_started(IPAddress, Port) ->
    io:format("Tcp listener started ~p: ~p~n", [IPAddress, Port]).

tcp_listener_stopped(IPAddress, Port) ->
    io:format("Tcp listener stopped ~p: ~p~n", [IPAddress, Port]).

start_client(Socket, Pid) ->
    {ok, {PeerIPAddress, _Port}} = inet:peername(Socket),
    io:format("Start one client process pid = ~p socket = ~p ip: ~p~n", [Pid, Socket, PeerIPAddress]).