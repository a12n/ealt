%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%% Internal protocol server. Dispatches translated event messages to connected
%%% clients. Listens for connections on TCP port 8642 by default.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_dispatcher).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_macros.hrl").

-behaviour(gen_server).

%% API
-export([dispatch/1, dispatch/2, start/0, start/1, start_link/0, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Internal functions
-export([accept_loop/2]).

-define(SERVER, ?MODULE). 

-record(state, {acceptor, clients = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Dispatch message to the listening clients.
%%
%% @spec dispatch(Message :: iolist()) -> ok
%% @end
%%--------------------------------------------------------------------
dispatch(Message) ->
    gen_server:cast(?SERVER, {dispatch, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Dispatch message to the particular client.
%%
%% @spec dispatch(Client :: port(), Message :: iolist()) -> ok
%% @end
%%--------------------------------------------------------------------
dispatch(Client, Message) ->
    gen_server:cast(?SERVER, {dispatch, Client, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Starts stand-alone server on default port.
%%
%% @spec start() ->
%%     {ok, Pid :: pid()} |
%%     ignore |
%%     {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start() ->
    start(?DEFAULT_DISPATCHER_PORT).

%%--------------------------------------------------------------------
%% @doc
%% Starts stand-alone server.
%%
%% @spec start(Port :: non_neg_integer()) ->
%%     {ok, Pid :: pid()} |
%%     ignore |
%%     {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start(Port) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server on default port in supervision tree.
%%
%% @spec start_link() ->
%%     {ok, Pid :: pid()} |
%%     ignore |
%%     {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(?DEFAULT_DISPATCHER_PORT).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server in supervision tree.
%%
%% @spec start_link(Port :: non_neg_integer()) ->
%%     {ok, Pid :: pid()} |
%%     ignore |
%%     {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%%
%% @spec init(Args :: list()) ->
%%     {ok, State} |
%%     {ok, State, Timeout} |
%%     ignore |
%%     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    Options = [binary, {active, false}, {packet, 0}, {reuseaddr, true}],
    {ok, Listen_Socket} = gen_tcp:listen(Port, Options),
    Acceptor = proc_lib:spawn_link(?MODULE, accept_loop, [self(), Listen_Socket]),
    State = #state{acceptor = Acceptor},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({accepted, Client}, State = #state{clients = Clients}) ->
    ?debugFmt("Client connection ~p accepted.", [Client]),
    ok = inet:setopts(Client, [{active, once}]),
    ok = ealt_event_manager:client_connected(Client),
    State_1 = State#state{clients = [Client | Clients]},
    {noreply, State_1};
handle_cast({dispatch, Message}, State = #state{clients = Clients}) ->
    ?debugFmt("Dispatching message ~p to clients.", [Message]),
    lists:foreach(fun(Client) -> ok = gen_tcp:send(Client, Message) end, Clients),
    {noreply, State};
handle_cast({dispatch, Client, Message}, State) ->
    ?debugFmt("Dispatching message ~p to client ~p.", [Message, Client]),
    ok = gen_tcp:send(Client, Message),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Client, _Data}, State) ->
    ?debugFmt("~p bytes from the client ~p ignored.", [size(_Data), Client]),
    ok = inet:setopts(Client, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, Client}, State = #state{clients = Clients}) ->
    ?debugFmt("Connection closed by client ~p.", [Client]),
    State_1 = State#state{clients = lists:delete(Client, Clients)},
    {noreply, State_1};
handle_info({tcp_error, Client}, State = #state{clients = Clients}) ->
    ?debugFmt("Client ~p connection error.", [Client]),
    ok = gen_tcp:close(Client),
    State_1 = State#state{clients = lists:delete(Client, Clients)},
    {noreply, State_1};
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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed.
%%
%% @spec code_change(Old_Vsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_Old_Vsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts new client connections and transfers them to the dispatcher.
%%
%% @spec accept_loop(Parent :: pid(), Listen_Socket :: port()) -> void()
%% @end
%%--------------------------------------------------------------------
accept_loop(Parent, Listen_Socket) ->
    ?debugMsg("Accepting connections."),
    case gen_tcp:accept(Listen_Socket) of
        {ok, Client_Socket} ->
            ok = gen_tcp:controlling_process(Client_Socket, Parent),
            gen_server:abcast(?SERVER, {accepted, Client_Socket});
        {error, _Reason} ->
            error
    end,
    accept_loop(Parent, Listen_Socket).
