%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_extractor).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_macros.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(MIN_REFRESH_RATE, 1000).
-define(MAX_REFRESH_RATE, 120000).

-define(PING_PACKET, <<16>>).

-record(state, {buffer = <<>>, keyframe_id = undefined, refresh_rate = ?MIN_REFRESH_RATE, socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts stand-alone packet extraction server.
%%
%% @spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts packet extraction server in supervision tree.
%%
%% @spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops packet extraction server.
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
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Options = [binary, {packet, 0}],
    {ok, Socket} = gen_tcp:connect(?LIVE_TIMING_HOST, ?LIVE_TIMING_PORT, Options),
    State = #state{socket = Socket},
    Timeout = ?MIN_REFRESH_RATE,
    {ok, State, Timeout}.

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
handle_cast(stop, State) ->
    Reason = normal,
    {stop, Reason, State};
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
handle_info({tcp, _Socket, Bytes}, State = #state{buffer = Buffer}) ->
    Buffer_1 = <<Buffer/bytes, Bytes/bytes>>,
    %% TODO: Do extract packets.
    State_1 = State#state{buffer = Buffer_1},
    Timeout = State_1#state.refresh_rate,
    {noreply, State_1, Timeout};
handle_info({Reason = tcp_closed, _Socket}, State) ->
    error_logger:error_msg("Server closed connection"),
    {stop, Reason, State};
handle_info({tcp_error, Reason}, State) ->
    error_logger:error_msg("Server connection error, reason \"~s\"", [Reason]),
    {stop, Reason, State};
handle_info(timeout, State = #state{refresh_rate = Refresh_Rate, socket = Socket}) ->
    ?debugMsg("Sending ping packet"),
    ok = gen_tcp:send(Socket, ?PING_PACKET),
    Timeout = Refresh_Rate,
    {noreply, State, Timeout}.

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
%% Retrieve packet keyframe of specified id. Parameter is a non negative integer
%% <em>Keyframe_Id</em> extracted from keyframe packet.
%%
%% @spec keyframe(Keyframe_Id :: non_neg_integer()) ->
%%     {ok, Bytes :: binary()} |
%%     {error, Reason :: term()} |
%%     {http_error, Reason :: term()}
%% @end
%%--------------------------------------------------------------------
keyframe(Keyframe_Id) ->
    Headers = [],
    HTTP_Options = [],
    Options = [{body_format, binary}],
    case httpc:request(get, {keyframe_url(Keyframe_Id), Headers}, HTTP_Options, Options, ealt) of
        {ok, {{_, Status_Code, _}, _, Content}} when ?is_success(Status_Code) ->
            {ok, Content};
        {ok, {{_, _, Reason}, _, _}} ->
            {http_error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This URL should be used then keyframe of packets should be
%% received. Requires non negative integer <em>Keyframe_Id</em>
%% parameter.
%%
%% @spec keyframe_url(Keyframe_Id :: non_neg_integer()) -> string()
%% @end
%%--------------------------------------------------------------------
keyframe_url(0) ->
    lists:flatten(io_lib:format("http://~s/keyframe.bin", [?LIVE_TIMING_HOST]));
keyframe_url(Keyframe_Id) ->
    lists:flatten(io_lib:format("http://~s/keyframe_~5..0B.bin",
                                [?LIVE_TIMING_HOST, Keyframe_Id])).
