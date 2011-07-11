%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.ru>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_packets).

-include("ealt_packets.hrl").

%% API
-export([read_packet/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
    case Bytes of
        <<Payload:Value/bytes, Other_Bytes>> ->
            {ok, 0, Payload, Other_Bytes};
        _ ->
            {error, no_match}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Extracts packet payload and constructs packet.
%%
%% @spec read_packet(Car_Id :: non_neg_integer(),
%%                   Type :: non_neg_integer(),
%%                   Value :: non_neg_integer(),
%%                   Bytes :: binary()) ->
%%     {ok, Packet, Other_Bytes :: binary} |
%%     {error, no_match}
%% @end
%%--------------------------------------------------------------------
read_packet(Car_Id, Type, Value, Bytes) ->
    Encrypted = is_payload_encrypted(Car_Id, Type),
    Read_Payload = read_payload_fun(Car_Id, Type),
    case Read_Payload(Value, Bytes) of
        {ok, Data, Payload, Other_Bytes} ->
            Packet = #packet{car_id = Car_Id,
                             type = Type,
                             data = Data,
                             payload = Payload,
                             encrypted = Encrypted},
            {ok, Packet, Other_Bytes};
        Error = {error, _Reason} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Payload extraction function for packet of type <em>Type</em> with <em>Car_Id</em>.
%%
%% @spec read_payload_fun(Car_Id :: non_neg_integer(), Type :: non_neg_integer()) -> fun()
%% @end
%%--------------------------------------------------------------------
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
    {ok, Value, <<>>, Bytes}.
