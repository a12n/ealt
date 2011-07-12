%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%% Packet conversions to internal representation.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_conversions).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_packets.hrl").

%% API
-export([convert_packet/1, convert_packet/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convert packet's payload to Erlang term.
%%
%% @spec convert_payload(Packet :: #packet{}) -> term()
%% @end
%%--------------------------------------------------------------------
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_EVENT_ID,
                       data = Data,
                       payload = {false, Bytes}}) ->
    Session =
        case Data of
            ?EVENT_ID_PACKET_PRACTICE ->
                practice;
            ?EVENT_ID_PACKET_QUALIFYING ->
                qualifying;
            ?EVENT_ID_PACKET_RACE ->
                race
        end,
    <<_Unknown, Event_Id/bytes>> = Bytes,
    {event_id, Session, binary_to_list(Event_Id)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_KEYFRAME,
                       payload = {false, Bytes}}) ->
    <<Keyframe_Id:16/little>> = Bytes,
    {keyframe, Keyframe_Id};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_VALID_MARKER,
                       data = Data}) ->
    {valid_marker, Data =:= 1};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_COMMENTARY,
                       payload = {false, Bytes}}) ->
    <<Byte_1, _Bits:6, Charset_Bit:1, Flush_Bit:1, Other_Bytes/bytes>> = Bytes,
    if Byte_1 >= 32 ->
            {commentary, latin1, true, binary_to_list(Bytes)};
       true ->
            Charset =
                case Charset_Bit of
                    0 -> utf8;
                    1 -> utf16le
                end,
            {commentary, Charset, Flush_Bit =:= 1, binary_to_list(Other_Bytes)}
    end;
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_REFRESH_RATE,
                       data = Data}) ->
    {refresh_rate, Data};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_NOTICE,
                       payload = {false, Bytes}}) ->
    {notice, binary_to_list(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_TIMESTAMP,
                       data = Data,
                       payload = {false, Bytes}}) ->
    <<Byte_1, Byte_2>> = Bytes,
    Seconds = (Data bsl 16) bor Byte_1 bor (Byte_2 bsl 8),
    {timestamp, Seconds};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_SESSION_CLOCK,
                       payload = {false, Bytes}}) ->
    {session_clock, convert_time(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_TRACK_TEMP,
                       payload = {false, Bytes}}) ->
    {track_temp, convert_integer(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_AIR_TEMP,
                       payload = {false, Bytes}}) ->
    {air_temp, convert_integer(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_WET_TRACK,
                       payload = {false, Bytes}}) ->
    {wet_track, convert_integer(Bytes) =:= 1};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_WIND_SPEED,
                       payload = {false, Bytes}}) ->
    {wind_speed, convert_float(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_HUMIDITY,
                       payload = {false, Bytes}}) ->
    {humidity, convert_integer(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_PRESSURE,
                       payload = {false, Bytes}}) ->
    {pressure, convert_float(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_WEATHER,
                       data = ?WEATHER_PACKET_WIND_DIRECTION,
                       payload = {false, Bytes}}) ->
    {wind_direction, convert_integer(Bytes)};
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_SPEED,
                       payload = {false, Bytes}}) ->
    <<Sub_Type, Other_Bytes/bytes>> = Bytes,
    case Sub_Type of
        ?SPEED_PACKET_SECTOR_1 ->
            {speed_sector_1, convert_speed_list(Other_Bytes)};
        ?SPEED_PACKET_SECTOR_2 ->
            {speed_sector_2, convert_speed_list(Other_Bytes)};
        ?SPEED_PACKET_SECTOR_3 ->
            {speed_sector_3, convert_speed_list(Other_Bytes)};
        ?SPEED_PACKET_SPEED_TRAP ->
            {speed_trap, convert_speed_list(Other_Bytes)};
        ?SPEED_PACKET_FASTEST_LAP_CAR ->
            {fastest_lap_car, convert_integer(Other_Bytes)};
        ?SPEED_PACKET_FASTEST_LAP_DRIVER ->
            {fastest_lap_driver, binary_to_list(Other_Bytes)};
        ?SPEED_PACKET_FASTEST_LAP_TIME ->
            {fastest_lap_time, convert_time(Other_Bytes)};
        ?SPEED_PACKET_FASTEST_LAP_LAP ->
            {fastest_lap, convert_integer(Other_Bytes)}
    end;
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_TRACK_STATUS,
                       data = ?TRACK_STATUS_PACKET_FLAG,
                       payload = {false, Bytes}}) ->
    case convert_integer(Bytes) of
        ?FLAG_GREEN ->
            green_flag;
        ?FLAG_YELLOW ->
            yellow_flag;
        ?FLAG_SCS ->
            scs;
        ?FLAG_SCD ->
            scd;
        ?FLAG_RED ->
            red_flag
    end;
convert_packet(#packet{car_id = 0,
                       type = ?SYSTEM_PACKET_COPYRIGHT,
                       payload = {false, Bytes}}) ->
    {copyright, binary_to_list(Bytes)};
convert_packet(#packet{car_id = Car_Id,
                       type = ?CAR_PACKET_POSITION_UPDATE,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {position_update, Car_Id, convert_integer(Bytes)};
convert_packet(#packet{car_id = Car_Id,
                       type = ?CAR_PACKET_POSITION_HISTORY,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {position_history, Car_Id, binary_to_list(Bytes)};
convert_packet(Packet) ->
    ?debugFmt("No convertion for packet ~p defined.", [Packet]),
    Packet.

%%--------------------------------------------------------------------
%% @doc
%% Convert packet's payload to Erlang term. Car packet payload is
%% different for practice, qualifying and race sessions. <em>Session<em>
%% parameter required to differentiate.
%%
%% @spec convert_payload(Session, Packet :: #packet{}) -> term()
%% where
%% Session = practice | qualifying | race
%% @end
%%--------------------------------------------------------------------

%%
%% Practice packets.
%%
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_POSITION,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, position}, Car_Id, convert_integer(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_NUMBER,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, number}, Car_Id, convert_integer(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_DRIVER,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, driver}, Car_Id, binary_to_list(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_BEST,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, best}, Car_Id, convert_time(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_GAP,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, gap}, Car_Id, convert_gap(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_SECTOR_1,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, sector_1}, Car_Id, convert_time(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_SECTOR_2,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, sector_2}, Car_Id, convert_time(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_SECTOR_3,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, sector_3}, Car_Id, convert_time(Bytes)};
convert_packet(practice,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_PRACTICE_LAP,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{practice, lap}, Car_Id, convert_integer(Bytes)};
%%
%% Qualifying packets.
%%
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_POSITION,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{qualifying, position}, Car_Id, convert_integer(Bytes)};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_NUMBER,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{qualifying, number}, Car_Id, convert_integer(Bytes)};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_DRIVER,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{qualifying, driver}, Car_Id, binary_to_list(Bytes)};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_PERIOD_1,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    %% FIXME
    {{qualifying, period_1}, Car_Id, Bytes};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_PERIOD_2,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    %% FIXME
    {{qualifying, period_2}, Car_Id, Bytes};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_PERIOD_3,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    %% FIXME
    {{qualifying, period_3}, Car_Id, Bytes};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_SECTOR_1,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{qualifying, sector_1}, Car_Id, convert_time(Bytes)};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_SECTOR_2,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{qualifying, sector_2}, Car_Id, convert_time(Bytes)};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_SECTOR_3,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{qualifying, sector_3}, Car_Id, convert_time(Bytes)};
convert_packet(qualifying,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_QUALIFYING_LAP,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{qualifying, lap}, Car_Id, convert_integer(Bytes)};
%%
%% Race packets.
%%
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_POSITION,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, position}, Car_Id, convert_integer(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_NUMBER,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, number}, Car_Id, convert_integer(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_DRIVER,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, driver}, Car_Id, binary_to_list(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_GAP,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, gap}, Car_Id, convert_gap(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_INTERVAL,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    %% FIXME
    {{race, interval}, Car_Id, Bytes};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_LAP_TIME,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, lap_time}, Car_Id, convert_time(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_SECTOR_1,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, sector_1}, Car_Id, convert_time(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_PIT_LAP_1,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    %% FIXME
    {{race, pit_lap_1}, Car_Id, Bytes};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_SECTOR_2,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, sector_2}, Car_Id, convert_time(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_PIT_LAP_2,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    %% FIXME
    {{race, pit_lap_2}, Car_Id, Bytes};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_SECTOR_3,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, sector_3}, Car_Id, convert_time(Bytes)};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_PIT_LAP_3,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    %% FIXME
    {{race, pit_lap_3}, Car_Id, Bytes};
convert_packet(race,
               #packet{car_id = Car_Id,
                       type = ?CAR_PACKET_RACE_NUM_PITS,
                       payload = {false, Bytes}}) when Car_Id > 0 ->
    {{race, num_pits}, Car_Id, convert_integer(Bytes)};
convert_packet(_Session, Packet) ->
    convert_packet(Packet).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
convert_gap(<<>>) ->
    undefined;
convert_gap(<<"LAP">>) ->
    {laps, 1};
convert_gap(<<L:1/bytes, $L>>) ->
    {laps, convert_integer(L)};
convert_gap(<<L:2/bytes, $L>>) ->
    {laps, convert_integer(L)};
convert_gap(Bytes) ->
    {time, convert_time(Bytes)}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
convert_integer(<<>>) ->
    undefined;
convert_integer(Bytes) ->
    list_to_integer(binary_to_list(Bytes)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
convert_float(<<>>) ->
    undefined;
convert_float(Bytes) ->
    list_to_float(binary_to_list(Bytes)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
convert_speed_list(Bytes) ->
    List_1 = binary_to_list(Bytes),
    List_2 = string:tokens(List_1, [$\r]),
    List_3 = ealt_utils:zip1(List_2),
    lists:map(fun({Driver, Speed}) -> {Driver, list_to_integer(Speed)} end, List_3).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
convert_time(<<>>) ->
    undefined;
convert_time(<<"RETIRED">>) ->
    retired;
convert_time(<<"STOP">>) ->
    stop;
convert_time(<<M:1/bytes, $:, S:2/bytes, $., Z:3/bytes>>) ->
    %% M:SS.ZZZ format
    {convert_integer(M), convert_integer(S), convert_integer(Z)};
convert_time(<<S:2/bytes, $., Z:1/bytes>>) ->
    %% SS.Z format
    {0, convert_integer(S), convert_integer(Z)};
convert_time(<<S:1/bytes, $., Z:1/bytes>>) ->
    %% S.Z format
    {0, convert_integer(S), convert_integer(Z)};
convert_time(<<M:2/bytes, $:, S:2/bytes>>) ->
    %% MM:SS format
    {convert_integer(M), convert_integer(S), 0};
convert_time(<<M:1/bytes, $:, S:2/bytes>>) ->
    %% M:SS format
    {convert_integer(M), convert_integer(S), 0}.
