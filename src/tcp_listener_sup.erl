%%%-------------------------------------------------------------------
%%% @author leonwly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 五月 2017 17:07
%%%-------------------------------------------------------------------
-module(tcp_listener_sup).
-author("leonwly").

-behaviour(supervisor).

%% API
-export([start_link/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(
    Port :: integer(),      % port to listen
    OnStartup :: {M :: atom(), F :: atom(), A :: list()},       % callback on startup
    OnShutdown :: {M :: atom(), F :: atom(), A :: list()},      % callback on shutdown
    AcceptCallback :: {M :: atom(), F :: atom(), A :: list()},  % callback on accept
    AcceptorCount :: integer()) ->                              % acceptor count
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Port, OnStartup, OnShutdown, AcceptCallback, AcceptorCount) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {Port, OnStartup, OnShutdown, AcceptCallback, AcceptorCount}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init({Port, OnStartup, OnShutdown, AcceptCallback, AcceptorCount}) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Restart = transient,

    AChild = {tcp_acceptor_sup, {tcp_acceptor_sup, start_link, [AcceptCallback]},
        Restart, infinity, supervisor, [tcp_acceptor_sup]},

    BChild = {tcp_listener, {tcp_listener, start_link, [Port, AcceptorCount, OnStartup, OnShutdown]},
        Restart, 100, worker, [tcp_listener]},
    
    {ok, {SupFlags, [AChild, BChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
