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

-import(ealt_utils, [binary_to_integer/1, binary_to_number/1]).

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
packet_to_term(Packet) ->
    packet_to_term(undefined, Packet).

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
-spec packet_to_term(session() | undefined, #packet{}) -> term().
packet_to_term(Session, #packet{ car = Car,
                                 type = Type,
                                 extra = Extra,
                                 payload = {plain, Payload} }) ->
    if Car =/= 0 ->
            car_packet_to_term(Session, Car, Type, Payload);
       true ->
            system_packet_to_term(Type, Extra, Payload)
    end.

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
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec binary_to_gap(binary()) ->
                           undefined | {laps, integer()} | {time, term()}.
binary_to_gap(<<>>) ->
    undefined;
binary_to_gap(<<"LAP">>) ->
    {laps, 1};
binary_to_gap(<<L:1/bytes, $L>>) ->
    {laps, binary_to_integer(L)};
binary_to_gap(<<L:2/bytes, $L>>) ->
    {laps, binary_to_integer(L)};
binary_to_gap(Binary) ->
    {time, binary_to_time(Binary)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec binary_to_speeds(binary()) -> [{binary(), integer()}].
binary_to_speeds(Binary) ->
    List = binary_to_list(Binary),
    Tokens = string:tokens(List, [$\r]),
    Pairs = ealt_utils:zip1(Tokens),
    lists:map(fun({Driver, Speed}) ->
                      {list_to_binary(Driver), list_to_integer(Speed)}
              end, Pairs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec binary_to_time(binary()) -> undefined | retired | stop |
                                 {integer(), integer(), integer()}.
binary_to_time(<<>>) ->
    undefined;
binary_to_time(<<"RETIRED">>) ->
    retired;
binary_to_time(<<"STOP">>) ->
    stop;
binary_to_time(<<M:1/bytes, $:, S:2/bytes, $., Z:3/bytes>>) ->
    %% M:SS.ZZZ format
    {binary_to_integer(M), binary_to_integer(S), binary_to_integer(Z)};
binary_to_time(<<S:2/bytes, $., Z:1/bytes>>) ->
    %% SS.Z format
    {0, binary_to_integer(S), binary_to_integer(Z)};
binary_to_time(<<S:1/bytes, $., Z:1/bytes>>) ->
    %% S.Z format
    {0, binary_to_integer(S), binary_to_integer(Z)};
binary_to_time(<<M:2/bytes, $:, S:2/bytes>>) ->
    %% MM:SS format
    {binary_to_integer(M), binary_to_integer(S), 0};
binary_to_time(<<M:1/bytes, $:, S:2/bytes>>) ->
    %% M:SS format
    {binary_to_integer(M), binary_to_integer(S), 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec car_packet_to_term(session() | undefined,
                         integer(),
                         integer(),
                         binary()) -> term().
%% Practice
car_packet_to_term(practice, Car, _Type = ?PRACTICE_BEST_TIME_PACKET, Payload) ->
    {best_time, Car, binary_to_time(Payload)};
%% Qualifying
car_packet_to_term(qualifying, Car, _Type = ?QUALIFYING_PERIOD_1_PACKET, Payload) ->
    {period_1, Car, Payload};                   % FIXME: period_1?
car_packet_to_term(qualifying, Car, _Type = ?QUALIFYING_PERIOD_2_PACKET, Payload) ->
    {period_2, Car, Payload};                   % FIXME: period_2?
car_packet_to_term(qualifying, Car, _Type = ?QUALIFYING_PERIOD_3_PACKET, Payload) ->
    {period_3, Car, Payload};                  % FIXME: period_3?
%% Race
car_packet_to_term(race, Car, _Type = ?RACE_INTERVAL_PACKET, Payload) ->
    {interval, Car, Payload};                   % FIXME: interval?
car_packet_to_term(race, Car, _Type = ?RACE_LAP_TIME_PACKET, Payload) ->
    {lap_time, Car, binary_to_time(Payload)};
car_packet_to_term(race, Car, _Type = ?RACE_PIT_LAP_1_PACKET, Payload) ->
    {pit_lap_1, Car, Payload};                  % FIXME: pit_lap_1?
car_packet_to_term(race, Car, _Type = ?RACE_PIT_LAP_2_PACKET, Payload) ->
    {pit_lap_2, Car, Payload};                  % FIXME: pit_lap_2?
car_packet_to_term(race, Car, _Type = ?RACE_PIT_LAP_3_PACKET, Payload) ->
    {pit_lap_3, Car, Payload};
car_packet_to_term(race, Car, _Type = ?RACE_N_PITS_PACKET, Payload) ->
    {n_pits, Car, binary_to_integer(Payload)};
%% Practice, qualifying
car_packet_to_term(Session, Car, Type, Payload)
  when (Session =:= practice and Type =:= ?PRACTICE_LAP_PACKET) or
       (Session =:= qualifying and Type =:= ?QUALIFYING_LAP_PACKET) ->
    {lap, Car, binary_to_integer(Payload)};
%% Practice, race
car_packet_to_term(Session, Car, Type, Payload)
  when (Session =:= practice and Type =:= ?PRACTICE_GAP_PACKET) or
       (Session =:= race and Type =:= ?RACE_GAP_PACKET) ->
    {gap, Car, binary_to_gap(Payload)};
%% Practice, qualifying, race
car_packet_to_term(Session, Car, Type, Payload)
  when (Session =:= practice and Type =:= ?PRACTICE_SECTOR_1_TIME_PACKET) or
       (Session =:= qualifying and Type =:= ?QUALIFYING_SECTOR_1_TIME_PACKET) or
       (Session =:= race and Type =:= ?RACE_SECTOR_1_TIME_PACKET) ->
    {sector_1_time, Car, binary_to_time(Payload)};
car_packet_to_term(Session, Car, Type, Payload)
  when (Session =:= practice and Type =:= ?PRACTICE_SECTOR_2_TIME_PACKET) or
       (Session =:= qualifying and Type =:= ?QUALIFYING_SECTOR_2_TIME_PACKET) or
       (Session =:= race and Type =:= ?RACE_SECTOR_2_TIME_PACKET) ->
    {sector_2_time, Car, binary_to_time(Payload)};
car_packet_to_term(Session, Car, Type, Payload)
  when (Session =:= practice and Type =:= ?PRACTICE_SECTOR_3_TIME_PACKET) or
       (Session =:= qualifying and Type =:= ?QUALIFYING_SECTOR_3_TIME_PACKET) or
       (Session =:= race and Type =:= ?RACE_SECTOR_3_TIME_PACKET) ->
    {sector_3_time, Car, binary_to_time(Payload)};
%% Practice, qualifying, race
car_packet_to_term(_Session, Car, _Type = ?POSITION_PACKET, Payload) ->
    {position, Car, binary_to_integer(Payload)};
car_packet_to_term(_Session, Car, _Type = ?NUMBER_PACKET, Payload) ->
    {number, Car, binary_to_integer(Payload)};
car_packet_to_term(_Session, Car, _Type = ?DRIVER_PACKET, Payload) ->
    {driver, Car, Payload};
%% Other
car_packet_to_term(_Session, Car, _Type = ?POSITION_UPDATE_PACKET, Payload) ->
    {position_update, Car, binary_to_integer(Payload)};
car_packet_to_term(_Session, Car, _Type = ?POSITION_HISTORY_PACKET, Payload) ->
    {position_history, Car, binary_to_list(Payload)}.

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
-spec system_packet_to_term(integer(),
                            integer() | undefined,
                            binary()) -> term().
system_packet_to_term(_Type = ?EVENT_PACKET, Extra, Payload) ->
    Session =
        case Extra of
            ?PRACTICE_EVENT ->
                practice;
            ?QUALIFYING_EVENT ->
                qualifying;
            ?RACE_EVENT ->
                race
        end,
    <<_Unknown, Id/bytes>> = Payload,
    {event, Id, Session};
system_packet_to_term(_Type = ?KEYFRAME_PACKET, _Extra, Payload) ->
    <<Id:16/little>> = Payload,
    {keyframe, Id};
system_packet_to_term(_Type = ?MARKER_PACKET, Extra, _Payload) ->
    {marker, Extra =:= 1};
system_packet_to_term(_Type = ?COMMENTARY_PACKET, _Extra, Payload) ->
    <<Char, _Unknown:6, Charset_Bit:1, Flush_Bit:1, Other_Bytes/bytes>> = Payload,
    if Char >= 32 ->
            {commentary, latin1, true, Payload};
       true ->
            Charset =
                case Charset_Bit of
                    0 -> utf8;
                    1 -> {utf16, little}
                end,
            {commentary, Charset, Flush_Bit =:= 1, Other_Bytes}
    end;
system_packet_to_term(_Type = ?REFRESH_TIME_PACKET, Extra, _Payload) ->
    {refresh_time, Extra};
system_packet_to_term(_Type = ?NOTICE_PACKET, _Extra, Payload) ->
    {notice, Payload};
system_packet_to_term(_Type = ?TIMESTAMP_PACKET, Extra, Payload) ->
    <<Low:16/little>> = Payload,
    {timestamp, (Extra bsl 16) bor Low};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?SESSION_CLOCK_PACKET, Payload) ->
    {session_clock, binary_to_time(Payload)};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?TRACK_TEMPERATURE_PACKET, Payload) ->
    {track_temperature, binary_to_number(Payload)};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?AIR_TEMPERATURE_PACKET, Payload) ->
    {air_temperature, binary_to_number(Payload)};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?WET_TRACK_PACKET, Payload) ->
    {wet_track, Payload =:= <<"1">>};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?WIND_SPEED_PACKET, Payload) ->
    {wind_speed, binary_to_number(Payload)};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?RELATIVE_HUMIDITY_PACKET, Payload) ->
    {relative_humidity, binary_to_number(Payload)};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?ATMOSPHERIC_PRESSURE_PACKET, Payload) ->
    {atmospheric_pressure, binary_to_number(Payload)};
system_packet_to_term(_Type = ?WEATHER_PACKET, _Extra = ?WIND_DIRECTION_PACKET, Payload) ->
    {wind_direction, binary_to_number(Payload)};
system_packet_to_term(_Type = ?SPEED_PACKET, _Extra, Payload) ->
    <<Speed, Other_Bytes>> = Payload,
    case Speed of
        ?SECTOR_1_SPEED_PACKET ->
            {sector_1_speed, binary_to_speeds(Other_Bytes)};
        ?SECTOR_2_SPEED_PACKET ->
            {sector_2_speed, binary_to_speeds(Other_Bytes)};
        ?SECTOR_3_SPEED_PACKET ->
            {sector_3_speed, binary_to_speeds(Other_Bytes)};
        ?SPEED_TRAP_PACKET ->
            {speed_trap, binary_to_speeds(Other_Bytes)};
        ?FASTEST_LAP_CAR_PACKET ->
            {fastest_lap_car, binary_to_integer(Other_Bytes)};
        ?FASTEST_LAP_DRIVER_PACKET ->
            {fastest_lap_driver, Other_Bytes};
        ?FASTEST_LAP_TIME_PACKET ->
            {fastest_lap_time, binary_to_time(Other_Bytes)};
        ?FASTEST_LAP_NUMBER_PACKET ->
            {fastest_lap_number, binary_to_integer(Other_Bytes)}
    end;
system_packet_to_term(_Type = ?SESSION_STATUS_PACKET, _Extra = ?SESSION_FLAG_PACKET, Payload) ->
    Flag =
        case binary_to_integer(Payload) of
            ?GREEN_FLAG ->
                green_flag;
            ?YELLOW_FLAG ->
                yellow_flag;
            ?SAFETY_CAR_STAND_BY ->
                safety_car_stand_by;
            ?SAFETY_CAR_DEPLOYED ->
                safety_car_deployed;
            ?RED_FLAG ->
                red_flag
        end,
    {session_status, Flag};
system_packet_to_term(_Type = ?COPYRIGHT_PACKET, _Extra, Payload) ->
    {copyright, Payload}.

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
