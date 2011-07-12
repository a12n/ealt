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
-include("ealt_packets.hrl").

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
%% @spec start() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}
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
handle_special_packet(#packet{car_id = 0,
                              type = ?SYSTEM_PACKET_REFRESH_RATE,
                              data = Seconds},
                      State) ->
    Refresh_Rate =
        case (Seconds * 1000) of
            0 -> ?MAX_REFRESH_RATE;
            N when N < ?MIN_REFRESH_RATE -> ?MIN_REFRESH_RATE;
            N when N > ?MAX_REFRESH_RATE -> ?MAX_REFRESH_RATE;
            N -> N
        end,
    ?debugFmt("Refresh rate set to ~p.", [Refresh_Rate]),
    State#state{refresh_rate = Refresh_Rate};
handle_special_packet(Packet = #packet{car_id = 0,
                                       type = ?SYSTEM_PACKET_KEYFRAME},
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
%% `true' if packet of specified <em>Type</em> with <em>Car_Id</em>
%% has encrypted payload.
%%
%% @spec is_payload_encrypted(Car_Id :: non_neg_integer(), Type :: non_neg_integer()) -> boolean()
%% @end
%%--------------------------------------------------------------------
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_EVENT_ID) -> false;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_KEYFRAME) -> false;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_VALID_MARKER) -> false;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_COMMENTARY) -> true;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_REFRESH_RATE) -> false;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_NOTICE) -> true;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_TIMESTAMP) -> true;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_WEATHER) -> true;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_SPEED) -> true;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_TRACK_STATUS) -> true;
is_payload_encrypted(_Car_Id = 0, _Type = ?SYSTEM_PACKET_COPYRIGHT) -> false;
is_payload_encrypted(Car_Id, _Type = ?CAR_PACKET_POSITION_UPDATE) when Car_Id > 0 -> false;
is_payload_encrypted(Car_Id, _Type = ?CAR_PACKET_POSITION_HISTORY) when Car_Id > 0-> true;
is_payload_encrypted(Car_Id, Type) when Car_Id > 0, Type >= 1, Type =< 13 -> true.

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
%% Packet with long payload has no packet <em>data</em>. It's `value'
%% field is the number of bytes to be extracted from the
%% <em>bytes</em> stream to get the <em>payload</em>.
%%
%% @spec read_long_payload(Value :: integer(), Bytes :: binary()) ->
%%     {ok, Data :: integer(), Payload :: binary(), Other_Bytes :: binary()} |
%%     {error, no_match}
%% @end
%%--------------------------------------------------------------------
read_long_payload(Value, Bytes) ->
    ?debugFmt("Long payload, value ~p, bytes ~P.",
              [Value, Bytes, 64]),
    case Bytes of
        <<Payload:Value/bytes, Other_Bytes/bytes>> ->
            {ok, 0, Payload, Other_Bytes};
        _ ->
            {error, no_match}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extracts packet from stream of <em>Bytes</em>.
%%
%% @spec read_packet(Bytes :: binary()) ->
%%     {ok, Packet, Other_Bytes :: binary} |
%%     {error, Reason :: term()}
%% where Reason = no_match
%% @end
%%--------------------------------------------------------------------
read_packet(<<Type_1:3, Car_Id:5, Value:7, Type_2:1, Bytes/bytes>>) ->
    Type = Type_1 bor (Type_2 bsl 3),
    read_packet(Car_Id, Type, Value, Bytes);
read_packet(_Bytes) ->
    {error, no_match}.

%%--------------------------------------------------------------------
%% @doc
%% Extracts packet payload and constructs packet. Timestamp system
%% packet is a special one as it is a long packet with 2 bytes of
%% payload despite of `value'.
%%
%% @spec read_packet(Car_Id :: non_neg_integer(),
%%                   Type :: non_neg_integer(),
%%                   Value :: non_neg_integer(),
%%                   Bytes :: binary()) ->
%%     {ok, Packet, Other_Bytes :: binary} |
%%     {error, no_match}
%% @end
%%--------------------------------------------------------------------
read_packet(Car_Id = 0, Type = ?SYSTEM_PACKET_TIMESTAMP, Value, Bytes) ->
    ?debugFmt("Reading timestamp packet, value ~p, bytes ~P.",
              [Value, Bytes, 64]),
    Encrypted = is_payload_encrypted(Car_Id, Type),
    Read_Payload = read_payload_fun(Car_Id, Type),
    case Read_Payload(2, Bytes) of
        {ok, _Data, Payload, Other_Bytes} ->
            Packet = #packet{car_id = Car_Id,
                             type = Type,
                             data = Value,
                             payload = {Encrypted, Payload}},
            {ok, Packet, Other_Bytes};
        Error = {error, _Reason} ->
            Error
    end;
read_packet(Car_Id, Type, Value, Bytes) ->
    ?debugFmt("Reading packet, car id ~p, type ~p, value ~p, bytes ~P.",
              [Car_Id, Type, Value, Bytes, 64]),
    Encrypted = is_payload_encrypted(Car_Id, Type),
    Read_Payload = read_payload_fun(Car_Id, Type),
    case Read_Payload(Value, Bytes) of
        {ok, Data, Payload, Other_Bytes} ->
            Packet = #packet{car_id = Car_Id,
                             type = Type,
                             data = Data,
                             payload = {Encrypted, Payload}},
            {ok, Packet, Other_Bytes};
        Error = {error, _Reason} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract packets from buffer while there are any.
%%
%% @spec read_packets(State :: #state{}) -> State_1 :: #state{}
%% @end
%%--------------------------------------------------------------------
read_packets(State = #state{buffer = Buffer}) ->
    case read_packet(Buffer) of
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

%%--------------------------------------------------------------------
%% @doc
%% Payload extraction function for packet of type <em>Type</em> with <em>Car_Id</em>.
%%
%% @spec read_payload_fun(Car_Id :: non_neg_integer(), Type :: non_neg_integer()) -> fun()
%% @end
%%--------------------------------------------------------------------
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_EVENT_ID) ->
    fun read_some_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_KEYFRAME) ->
    fun read_some_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_VALID_MARKER) ->
    fun read_zero_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_COMMENTARY) ->
    fun read_long_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_REFRESH_RATE) ->
    fun read_zero_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_NOTICE) ->
    fun read_long_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_TIMESTAMP) ->
    fun read_long_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_WEATHER) ->
    fun read_some_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_SPEED) ->
    fun read_long_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_TRACK_STATUS) ->
    fun read_some_payload/2;
read_payload_fun(_Car_Id = 0, _Type = ?SYSTEM_PACKET_COPYRIGHT) ->
    fun read_long_payload/2;
read_payload_fun(Car_Id, _Type = ?CAR_PACKET_POSITION_UPDATE) when Car_Id > 0 ->
    fun read_zero_payload/2;
read_payload_fun(Car_Id, _Type = ?CAR_PACKET_POSITION_HISTORY) when Car_Id > 0 ->
    fun read_long_payload/2;
read_payload_fun(Car_Id, Type) when Car_Id > 0, Type >= 1, Type =< 13 ->
    fun read_some_payload/2.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% `value' field in packets with some payload consists of 3 bits of
%% <em>data</em> and 4 bits of payload length. If payload length is
%% `2#1111`, there is no <em>payload</em>. If it's not, that number of
%% bytes is extracted from the stream of <em>bytes</em>.
%%
%% @spec read_some_payload(Value :: integer(), Bytes :: binary()) ->
%%     {ok, Data :: integer(), Payload :: binary(), Other_Bytes :: binary()} |
%%     {error, no_match}
%% @end
%%--------------------------------------------------------------------
read_some_payload(Value, Bytes) ->
    Data = Value band 2#111,
    Length = Value bsr 3,
    ?debugFmt("Some payload, value ~p (data ~p, length ~p), bytes ~P.",
              [Value, Data, Length, Bytes, 64]),
    case Length of
        2#1111 ->
            {ok, Data, <<>>, Bytes};
        _ ->
            case Bytes of
                <<Payload:Length/bytes, Other_Bytes/bytes>> ->
                    {ok, Data, Payload, Other_Bytes};
                _ ->
                    {error, no_match}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The only <em>data</em> in zero payload packet is the `value' field
%% of the packet. Resulting <em>payload</em> is empty, and stream
%% <em>bytes</em> are unchanged.
%%
%% @spec read_zero_payload(Value :: integer(), Bytes :: binary()) ->
%%     {ok, Data :: integer(), Payload :: binary(), Other_Bytes :: binary()}
%% @end
%%--------------------------------------------------------------------
read_zero_payload(Value, Bytes) ->
    ?debugFmt("Zero payload, value ~p, bytes ~P.",
              [Value, Bytes, 64]),
    {ok, Value, <<>>, Bytes}.
