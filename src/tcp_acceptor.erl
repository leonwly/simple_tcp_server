%%%-------------------------------------------------------------------
%%% @author leonwly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 五月 2017 17:53
%%%-------------------------------------------------------------------
-module(tcp_acceptor).

-include("base_define.hrl").

-author("leonwly").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([
    get_proc_name/1,
    enable_connect/1,
    disable_connect/1
]).

-define(SERVER, ?MODULE).

-record(state, {
    callback,
    sock,
    ref,
    disable_connect
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
start_link(Callback, LSock, AcceptorIndex) ->
    gen_server:start_link(?MODULE, {Callback, LSock, AcceptorIndex}, []).

%% @doc return process name by AcceptorIndex
get_proc_name(AcceptorIndex)->
    list_to_atom("acceptor_"++integer_to_list(AcceptorIndex)).

enable_connect(NamedProc) ->
    case erlang:whereis(NamedProc) of
        ?ERLNULL -> ignor;
        Pid -> gen_server:call(Pid, enable_connect)
    end.

disable_connect(NamedProc) ->
    case erlang:whereis(NamedProc) of
        ?ERLNULL -> ignor;
        Pid -> gen_server:call(Pid, disable_connect)
    end.

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
init({Callback, LSock, AcceptorIndex}) ->
    erlang:register(get_proc_name(AcceptorIndex), self()),
    gen_server:cast(self(), accept),
    {ok, #state{callback = Callback, sock = LSock,disable_connect = false}}.

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
handle_call(enable_connect, _From, State) ->
    Reply = State,
    {reply, Reply, State#state{disable_connect = false}};

handle_call(disable_connect, _From, State) ->
    Reply = State,
    {reply, Reply, State#state{disable_connect = true}};

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
handle_info({inet_async, LSock, Ref, {ok, Sock}}, #state{callback = {M,F,A}, sock = LSock, ref = Ref, disable_connect = Disable}=State) ->
    {ok, Mod} = inet_db:lookup_socket(LSock),
    inet_db:register_socket(Sock, Mod),
    try
        {Address, Port} = inet_op(fun() -> inet:sockname(LSock) end),
        {PeerAddress, PeerPort} = inet_op(fun() -> inet:peername(Sock) end),
        {ok, ChildPid} = supervisor:start_child(player_session_sup, []),
        ok = gen_tcp:controlling_process(ChildPid),
        case Disable of
            true ->
                todo;
            false ->
                todo
        end,
        apply(M, F, A ++ [Sock, ChildPid])
    catch
        {inet_error, Reason} ->
            gen_tcp:close(Sock),
            io:format("Unable to accept tcp connection: ~p~n", [Reason]);
        E ->
            io:format("Unable to accept tcp connection: ~p~n", [E])
    end,
    accept(State);

handle_info({inet_async, LSock, Ref, {error, closed}}, #state{sock = LSock, ref = Ref}=State) ->
    {stop, normal, State};

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
terminate(_Reason, _State) ->
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
throw_on_error(E, Thunk) ->
    case Thunk() of
        {error, Reason} -> throw({E, Reason});
        {ok, Res} -> Res;
        Res -> Res
    end.

inet_op(F) -> throw_on_error(inet_error, F).

accept(State = #state{sock = LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref = Ref}};
        Error -> {stop, {cannot_accept, Error}, State}
    end.
