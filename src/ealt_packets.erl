%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% TODO
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_packets).

%% API
-export([descramble_payload/3, packet_to_term/1, packet_to_term/2,
         read_packet/1]).

-include("ealt_packets.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Decrypt scrambled payload <em>Bytes</em>.
%% @end
%%--------------------------------------------------------------------
-spec descramble_payload(binary(), integer(), integer()) ->
                                {binary(), integer()}.
descramble_payload(Bytes, Key, Mask) ->
    descramble_payload(<<>>, Bytes, Key, Mask).

%%--------------------------------------------------------------------
%% @doc
%% Convert <em>Packet</em> to Erlang term of packet's internal
%% representation.
%% @end
%%--------------------------------------------------------------------
-spec packet_to_term(#packet{}) -> term().
packet_to_term(_Packet) ->
    %% TODO
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Convert <em>Packet</em> to Erlang term of packet's internal
%% representation.
%%
%% Car packet payload may be different for practice, qualifying and race
%% sessions. <em>Session</em> parameter is required for
%% differentiation.
%% @end
%%--------------------------------------------------------------------
-spec packet_to_term(session(), #packet{}) -> term().
packet_to_term(practice, _Packet) ->
    %% TODO
    undefined;
packet_to_term(qualifying, _Packet) ->
    %% TODO
    undefined;
packet_to_term(race, _Packet) ->
    %% TODO
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec read_packet(binary()) -> {ok, #packet{}, binary()} |
                              {more, integer() | undefined} |
                              {error, term()}.
read_packet(<<Type_1:3, Car:5, Extra:7, Type_2:1, Other/bytes>>) ->
    Type = Type_1 bor (Type_2 bsl 3),
    read_packet(Car, Type, Extra, Other);
read_packet(_Bytes) ->
    {more, undefined}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decrypt <em>Encr</em> bytes of payload appending to the provided
%% <em>Decr</em> binary.
%% @end
%%--------------------------------------------------------------------
-spec descramble_payload(binary(), binary(), integer(), integer()) ->
                                {binary(), integer()}.
descramble_payload(Decr, <<>>, _Key, Mask) ->
    {Decr, Mask};
descramble_payload(Decr, <<Encr_Byte, Next_Encr/bytes>>, Key, Mask) ->
    Next_Mask = ((Mask bsr 1) bxor (Mask band 1) * Key) band 16#ffffffff,
    Byte = Encr_Byte bxor (Next_Mask band 16#ff),
    Next_Decr = <<Decr/bytes, Byte>>,
    descramble_payload(Next_Decr, Next_Encr, Key, Next_Mask).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec payload_kind(integer(), integer()) -> {plain | scrambled,
                                            long | short | zero}.
payload_kind(_Car = 0, _Type = ?EVENT_PACKET) ->
    {plain, short};
payload_kind(_Car = 0, _Type = ?KEYFRAME_PACKET) ->
    {plain, short};
payload_kind(_Car = 0, _Type = ?MARKER_PACKET) ->
    {plain, zero};
payload_kind(_Car = 0, _Type = ?COMMENTARY_PACKET) ->
    {scrambled, long};
payload_kind(_Car = 0, _Type = ?REFRESH_TIME_PACKET) ->
    {plain, zero};
payload_kind(_Car = 0, _Type = ?NOTICE_PACKET) ->
    {scrambled, long};
payload_kind(_Car = 0, _Type = ?TIMESTAMP_PACKET) ->
    {scrambled, long};
payload_kind(_Car = 0, _Type = ?WEATHER_PACKET) ->
    {scrambled, short};
payload_kind(_Car = 0, _Type = ?SPEED_PACKET) ->
    {scrambled, long};
payload_kind(_Car = 0, _Type = ?SESSION_STATUS_PACKET) ->
    {scrambled, short};
payload_kind(_Car = 0, _Type = ?COPYRIGHT_PACKET) ->
    {plain, long};
payload_kind(Car, _Type = ?POSITION_UPDATE_PACKET) when Car > 0 ->
    {plain, zero};
payload_kind(Car, _Type = ?POSITION_HISTORY_PACKET) when Car > 0 ->
    {scrambled, long};
payload_kind(Car, Type) when Car > 0, Type >= 1, Type =< 13 ->
    {scrambled, short}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read packet payload. Timestamp system packet is a special case as
%% it is a long packet with 2 bytes of payload despite of <em>Extra</em>
%% field. Packet's `extra' field is unchanged unlike to other long
%% packets.
%% @end
%%--------------------------------------------------------------------
read_packet(Car = 0, Type = ?TIMESTAMP_PACKET, Extra, Bytes) ->
    {Scrambled, Kind} = payload_kind(Car, Type),
    case read_payload(Kind, 2, Bytes) of
        {ok, _Data, Payload, Other_Bytes} ->
            Packet = #packet{ car = Car,
                              type = Type,
                              extra = Extra,
                              payload = {Scrambled, Payload} },
            {ok, Packet, Other_Bytes};
        Other ->
            Other
    end;
read_packet(Car, Type, Extra, Bytes) ->
    {Scrambled, Kind} = payload_kind(Car, Type),
    case read_payload(Kind, Extra, Bytes) of
        {ok, Data, Payload, Other_Bytes} ->
            Packet = #packet{ car = Car,
                              type = Type,
                              extra = Data,
                              payload = {Scrambled, Payload} },
            {ok, Packet, Other_Bytes};
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read packet payload of <em>Kind</em>. Kind is `long', `short' or
%% `zero'.
%%
%% Packet with long payload has no extra data. It's <em>Extra</em>
%% field is the number of bytes to be extracted from <em>Bytes</em>
%% stream to get the payload.
%%
%% <em>Extra</em> field in short payload packet consists of 3
%% bits of data and 4 bits of payload length. If payload length is
%% `2#1111`, there is no payload. If it's not, that number of bytes
%% should be extracted from the stream of <em>Bytes</em>.
%%
%% The only data in zero payload packet is the <em>Extra</em> field.
%% Resulting payload is empty, and stream <em>Bytes</em> are unchanged.
%% @end
%%--------------------------------------------------------------------
-spec read_payload(long | short | zero, integer(), binary()) ->
                          {ok, integer() | undefined, binary(), binary()} |
                          {more, integer() | undefined}.
read_payload(long, Extra, Bytes) ->
    case Bytes of
        <<Payload:Extra/bytes, Other_Bytes/bytes>> ->
            {ok, undefined, Payload, Other_Bytes};
        _Other ->
            {more, undefined}
    end;
read_payload(short, Extra, Bytes) ->
    Data = Extra band 2#111,
    Length = Extra bsr 3,
    case Length of
        2#1111 ->
            {ok, Data, <<>>, Bytes};
        _Other_Length ->
            case Bytes of
                <<Payload:Length/bytes, Other_Bytes/bytes>> ->
                    {ok, Data, Payload, Other_Bytes};
                _Other ->
                    {more, undefined}
            end
    end;
read_payload(zero, Extra, Bytes) ->
    {ok, Extra, <<>>, Bytes}.
