%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_extractor).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_internal.hrl").
-include("ealt_packets.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MIN_REFRESH_RATE, 1000).
-define(MAX_REFRESH_RATE, 120000).

-define(PING_PACKET, <<16>>).

-record(state, {buffer = <<>>, keyframe_id = undefined, refresh_rate = ?MIN_REFRESH_RATE, socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts packet extraction server in supervision tree.
%%
%% @spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    State_1 = State#state{buffer = <<Buffer/bytes, Bytes/bytes>>},
    State_2 = read_packets(State_1),
    Timeout = State_2#state.refresh_rate,
    {noreply, State_2, Timeout};
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
%% Handles refresh rate and keyframe packets. Modifies <em>State</em>
%% accordingly.
%%
%% @spec handle_special_packet(Packet :: #packet{}, State :: #state{}) ->
%%     State_1 :: #state{}
%% @end
%%--------------------------------------------------------------------
handle_special_packet(#packet{car = 0,
                              type = ?REFRESH_TIME_PACKET,
                              extra = Seconds}, State) ->
    Refresh_Rate =
        case (Seconds * 1000) of
            0 -> ?MAX_REFRESH_RATE;
            N when N < ?MIN_REFRESH_RATE -> ?MIN_REFRESH_RATE;
            N when N > ?MAX_REFRESH_RATE -> ?MAX_REFRESH_RATE;
            N -> N
        end,
    ?debugFmt("Refresh rate set to ~p.", [Refresh_Rate]),
    State#state{refresh_rate = Refresh_Rate};
handle_special_packet(Packet = #packet{car = 0,
                                       type = ?KEYFRAME_PACKET},
                      State = #state{buffer = Buffer}) ->
    {keyframe, Keyframe_Id} = ealt_conversions:convert_packet(Packet),
    ?debugFmt("Keyframe packet with keyframe id ~p.", [Keyframe_Id]),
    case State#state.keyframe_id of
        undefined ->
            {ok, Bytes} = keyframe(Keyframe_Id),
            ?debugFmt("Injecting ~p bytes of data from keyframe ~p.",
                      [size(Bytes), Keyframe_Id]),
            State#state{keyframe_id = Keyframe_Id,
                        buffer = <<Bytes/bytes, Buffer/bytes>>};
        _Id ->
            State#state{keyframe_id = Keyframe_Id}
    end;
handle_special_packet(_Packet, State) ->
    State.

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
    ?debugFmt("Receiving keyframe ~p.", [Keyframe_Id]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract packets from buffer while there are any.
%%
%% @spec read_packets(State :: #state{}) -> State_1 :: #state{}
%% @end
%%--------------------------------------------------------------------
read_packets(State = #state{buffer = Buffer}) ->
    case ealt_packets:read_packet(Buffer) of
        {ok, Packet, Buffer_1} ->
            ?debugFmt("Packet ~p extracted.", [Packet]),
            State_1 = State#state{buffer = Buffer_1},
            State_2 = handle_special_packet(Packet, State_1),
            ealt_decoder:process_packet(Packet),
            read_packets(State_2);
        {error, no_match} ->
            ?debugMsg("No more packets in buffer."),
            State
    end.
