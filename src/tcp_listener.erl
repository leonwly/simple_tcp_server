%%%-------------------------------------------------------------------
%%% @author leonwly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 五月 2017 17:15
%%%-------------------------------------------------------------------
-module(tcp_listener).

-include("network_define.hrl").

-author("leonwly").

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

%%-define(OPEN_GATE_TIME,10*1000).
%%-define(OPEN_GATE_DOOR,enable_connect()).
-ifdef(debug).
    -define(OPEN_GATE_TIME,10*1000).
    -define(OPEN_GATE_DOOR,enable_connect()).
-else.
    -define(OPEN_GATE_TIME,10*1000).
    -define(OPEN_GATE_DOOR,disable_connect()).
-endif.

-record(state, {
    listen_socket,
    on_startup,
    on_shutdown,
    acceptors
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(
    Port :: integer(),      % port to listen
    AcceptorCount :: integer(),                                 % acceptor count
    OnStartup :: {M :: atom(), F :: atom(), A :: list()},       % callback on startup
    OnShutdown :: {M :: atom(), F :: atom(), A :: list()}) ->   % callback on shutdown
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()} .
start_link(Port, AcceptorCount, OnStartup, OnShutdown) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Port, AcceptorCount, OnStartup, OnShutdown}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Port, AcceptorCount, {M, F, A}=OnStartup, OnShutdown}) ->
    process_flag(trap_exit, true),
    Opts = ?TCP_OPTIONS,
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            SeqList = lists:seq(1, AcceptorCount),
            Fun =
                fun(AccIndex, Acc) ->
                    {ok, _Pid} = supervisor:start_child(tcp_acceptor_sup, [ListenSocket, AccIndex]),
                    AcceptorName = tcp_acceptor:get_proc_name(AccIndex),
                    erlang:send_after(?OPEN_GATE_TIME, ?MODULE, enable_connect),
                    [AcceptorName|Acc]
                end,
            AccProcs = lists:foldl(Fun, [], SeqList),
            ?OPEN_GATE_DOOR,
            apply(M, F, A ++ [ListenSocket, Port]),
            {ok, #state{
                listen_socket = ListenSocket,
                on_startup = OnStartup,
                on_shutdown = OnShutdown,
                acceptors = AccProcs
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(enable_connect, #state{acceptors = AccProcs}=State) ->
    lists:foreach(
        fun(AcceptorName) ->
            R = tcp_acceptor:enable_connect(AcceptorName),
            io:format("Acceptor state: ~p~n", [R])
        end, AccProcs
    ),
    {noreply, State};

handle_info(disable_connect, #state{acceptors = AccProcs}=State) ->
    lists:foreach(
        fun(AcceptorName) ->
            R = tcp_acceptor:disable_connect(AcceptorName),
            io:format("Acceptor state: ~p~n", [R])
        end, AccProcs
    ),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{listen_socket = LSock, on_shutdown = {M,F,A}}) ->
    {ok, {_IPAddress, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
    apply(M, F, A ++ [LSock, Port]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
enable_connect() ->
    erlang:send_after(0, ?MODULE, enable_connect).

disable_connect() ->
    erlang:send_after(0, ?MODULE, disable_connect).
