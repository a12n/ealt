%%%-------------------------------------------------------------------
%%% @copyright 2011–2012, Anton Yabchinskiy <arn@users.berlios.de>
%%% @doc
%%% Decoded packet to message translation module.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_messages).

%% Types
-export_type([car/0, gap/0, lap_sector/0, lap_time/0,
              qualifying_period/0, session/0, session_status/0]).
-export_type([car_message/0, message/0, system_message/0]).

%% API
-export([packet_to_message/2]).

-import(ealt_utils, [binary_to_integer/1, binary_to_number/1]).

-include("ealt.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type car() :: non_neg_integer().

-type gap() :: {time, lap_time()} | {laps, pos_integer()}.

-type lap_sector() :: 1..3.

-type lap_time() ::
        {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-type qualifying_period() :: 1..3.

-type session() :: practice | qualifying | race.

-type session_status() ::
        green_flag | yellow_flag | scs | scd | red_flag.

%%--------------------------------------------------------------------

-type car_message() ::
        {best_time, car(), lap_time()} |
        {period_time, qualifying_period(), car(), lap_time()} |
        {interval, car(), gap()} |
        {lap, car(), pos_integer()} |
        {lap_time, car(), lap_time()} |
        {n_pits, car(), non_neg_integer()} |
        {gap, car(), gap()} |
        {sector_time, lap_sector(), car(), lap_time()} |
        {position, car(), pos_integer()} |
        {number, car(), pos_integer()} |
        {driver, car(), binary()} |
        %% {position_update, car(), pos_integer()} |
        {position_history, car(), [pos_integer()]}.

-type message() :: car_message() | system_message().

-type system_message() ::
        {event, binary(), session()} |
        {keyframe, integer()} |
        {marker, boolean()} |
        {commentary, boolean(), binary()} |
        {refresh_time, non_neg_integer()} |
        {notice, binary()} |
        {timestamp, non_neg_integer()} |
        {session_clock, calendar:time()} |
        {track_temperature, number()} |
        {air_temperature, number()} |
        {wet_track, boolean()} |
        {wind_speed, number()} |
        {relative_humidity, number()} |
        {atmospheric_pressure, number()} |
        {wind_direction, number()} |
        {sector_speed, lap_sector(), [{binary(), number()}]} |
        {speed_trap, [{binary(), number()}]} |
        {fastest_lap_car, car()} |
        {fastest_lap_driver, binary()} |
        {fastest_lap_time, lap_time()} |
        {fastest_lap_lap, pos_integer()} |
        {session_status, session_status()} |
        {copyright, binary()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec packet_to_message(session(), ealt_packets:packet()) -> message().
packet_to_message(Session, {Car, Type, Extra, {plain, Payload}}) ->
    case Car of
        0 ->
            to_system_message(Type, Extra, Payload);
        _Other ->
            to_car_message(Session, Car, Type, Payload)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec to_car_message(session(), car(), integer(), binary()) ->
                            car_message() | undefined.
%% Practice
to_car_message(practice, Car, ?PRACTICE_BEST_TIME_PACKET, Payload) ->
    {best_time, Car, binary_to_lap_time(Payload)};

%% Qualifying
to_car_message(qualifying, Car, ?QUALIFYING_PERIOD_1_TIME_PACKET, Payload) ->
    {period_time, 1, Car, binary_to_lap_time(Payload)};

to_car_message(qualifying, Car, ?QUALIFYING_PERIOD_2_TIME_PACKET, Payload) ->
    {period_time, 2, Car, binary_to_lap_time(Payload)};

to_car_message(qualifying, Car, ?QUALIFYING_PERIOD_3_TIME_PACKET, Payload) ->
    {period_time, 3, Car, binary_to_lap_time(Payload)};

%% Race
to_car_message(race, Car, ?RACE_INTERVAL_PACKET, Payload) ->
    %% "The Gap & Interval column for the leading driver always shows
    %% the number of laps completed, rather than the Gap and Interval
    %% information for that driver."
    case binary_to_integer(Payload) of
        undefined ->
            {interval, Car, binary_to_gap(Payload)};
        Lap ->
            {lap, Car, Lap}
    end;

to_car_message(race, Car, ?RACE_LAP_TIME_PACKET, Payload) ->
    {lap_time, Car, binary_to_lap_time(Payload)};

to_car_message(race, _Car, ?RACE_PIT_LAP_1_PACKET, _Payload) ->
    %% {pit_lap_1, Car, Payload};
    undefined;

to_car_message(race, _Car, ?RACE_PIT_LAP_2_PACKET, _Payload) ->
    %% {pit_lap_2, Car, Payload};
    undefined;

to_car_message(race, _Car, ?RACE_PIT_LAP_3_PACKET, _Payload) ->
    %% {pit_lap_3, Car, Payload};
    undefined;

to_car_message(race, Car, ?RACE_N_PITS_PACKET, Payload) ->
    {n_pits, Car, binary_to_integer(Payload)};

%% Practice, qualifying
to_car_message(Session, Car, Type, Payload)
  when ((Session =:= practice) and (Type =:= ?PRACTICE_LAP_PACKET)) or
       ((Session =:= qualifying) and (Type =:= ?QUALIFYING_LAP_PACKET)) ->
    {lap, Car, binary_to_integer(Payload)};

%% Practice, race
to_car_message(Session, Car, Type, Payload)
  when ((Session =:= practice) and (Type =:= ?PRACTICE_GAP_PACKET)) or
       ((Session =:= race) and (Type =:= ?RACE_GAP_PACKET)) ->
    case Payload of
        <<"LAP">> ->
            %% "The Gap & Interval column for the leading driver always shows
            %% the number of laps completed, rather than the Gap and Interval
            %% information for that driver."
            undefined;
        _Other ->
            {gap, Car, binary_to_gap(Payload)}
    end;

%% Practice, qualifying, race
to_car_message(Session, Car, Type, Payload)
  when ((Session =:= practice) and (Type =:= ?PRACTICE_SECTOR_1_TIME_PACKET)) or
       ((Session =:= qualifying) and (Type =:= ?QUALIFYING_SECTOR_1_TIME_PACKET)) or
       ((Session =:= race) and (Type =:= ?RACE_SECTOR_1_TIME_PACKET)) ->
    {sector_time, 1, Car, binary_to_lap_time(Payload)};

to_car_message(Session, Car, Type, Payload)
  when ((Session =:= practice) and (Type =:= ?PRACTICE_SECTOR_2_TIME_PACKET)) or
       ((Session =:= qualifying) and (Type =:= ?QUALIFYING_SECTOR_2_TIME_PACKET)) or
       ((Session =:= race) and (Type =:= ?RACE_SECTOR_2_TIME_PACKET)) ->
    {sector_time, 2, Car, binary_to_lap_time(Payload)};

to_car_message(Session, Car, Type, Payload)
  when ((Session =:= practice) and (Type =:= ?PRACTICE_SECTOR_3_TIME_PACKET)) or
       ((Session =:= qualifying) and (Type =:= ?QUALIFYING_SECTOR_3_TIME_PACKET)) or
       ((Session =:= race) and (Type =:= ?RACE_SECTOR_3_TIME_PACKET)) ->
    {sector_time, 3, Car, binary_to_lap_time(Payload)};

%% Practice, qualifying, race
to_car_message(_Session, Car, ?POSITION_PACKET, Payload) ->
    {position, Car, binary_to_integer(Payload)};

to_car_message(_Session, Car, ?NUMBER_PACKET, Payload) ->
    {number, Car, binary_to_integer(Payload)};

to_car_message(_Session, Car, ?DRIVER_PACKET, Payload) ->
    {driver, Car, Payload};

%% Other
to_car_message(_Session, Car, ?POSITION_UPDATE_PACKET, Payload) ->
    %% {position_update, Car, binary_to_integer(Payload)};
    {position, Car, binary_to_integer(Payload)};

to_car_message(_Session, Car, ?POSITION_HISTORY_PACKET, Payload) ->
    {position_history, Car, binary_to_list(Payload)};

to_car_message(Session, Car, Type, Payload) ->
    error_logger:error_msg("Unknown car packet (session ~p, car ~p, type ~p, payload ~p)~n",
                           [Session, Car, Type, Payload]),
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec to_system_message(integer(), integer(), binary()) ->
                               system_message() | undefined.
to_system_message(?EVENT_PACKET, Extra, Payload) ->
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

to_system_message(?KEYFRAME_PACKET, _Extra, Payload) ->
    <<Id:16/little>> = Payload,
    {keyframe, Id};

to_system_message(?MARKER_PACKET, Extra, _Payload) ->
    {marker, Extra =:= 1};

to_system_message(?COMMENTARY_PACKET, _Extra, Payload) ->
    case Payload of
        <<Char, _Other>> when Char >= 32 ->
            {commentary, true, unicode:characters_to_binary(Payload, latin1, utf8)};
        <<_Unknown:14, Encoding_Bit:1, Flush_Bit:1, Other_Bytes/bytes>> ->
            Text =
                case Encoding_Bit of
                    0 -> Other_Bytes;
                    1 -> unicode:characters_to_binary(Other_Bytes, {utf16, little}, utf8)
                end,
            {commentary, Flush_Bit =:= 1, Text}
    end;

to_system_message(?REFRESH_TIME_PACKET, Extra, _Payload) ->
    {refresh_time, Extra};

to_system_message(?NOTICE_PACKET, _Extra, Payload) ->
    {notice, Payload};

to_system_message(?TIMESTAMP_PACKET, Extra, Payload) ->
    <<Low:16/little>> = Payload,
    {timestamp, (Extra bsl 16) bor Low};

to_system_message(?WEATHER_PACKET, ?SESSION_CLOCK_PACKET, Payload) ->
    {session_clock, binary_to_time(Payload)};

to_system_message(?WEATHER_PACKET, ?TRACK_TEMPERATURE_PACKET, Payload) ->
    {track_temperature, binary_to_number(Payload)};

to_system_message(?WEATHER_PACKET, ?AIR_TEMPERATURE_PACKET, Payload) ->
    {air_temperature, binary_to_number(Payload)};

to_system_message(?WEATHER_PACKET, ?WET_TRACK_PACKET, Payload) ->
    {wet_track, Payload =:= <<"1">>};

to_system_message(?WEATHER_PACKET, ?WIND_SPEED_PACKET, Payload) ->
    {wind_speed, binary_to_number(Payload)};

to_system_message(?WEATHER_PACKET, ?RELATIVE_HUMIDITY_PACKET, Payload) ->
    {relative_humidity, binary_to_number(Payload)};

to_system_message(?WEATHER_PACKET, ?ATMOSPHERIC_PRESSURE_PACKET, Payload) ->
    {atmospheric_pressure, binary_to_number(Payload)};

to_system_message(?WEATHER_PACKET, ?WIND_DIRECTION_PACKET, Payload) ->
    {wind_direction, binary_to_number(Payload)};

to_system_message(?SPEED_PACKET, _Extra, Payload) ->
    <<Field, Other_Bytes/bytes>> = Payload,
    case Field of
        ?SECTOR_1_SPEED_PACKET ->
            {sector_speed, 1, binary_to_speed_list(Other_Bytes)};
        ?SECTOR_2_SPEED_PACKET ->
            {sector_speed, 2, binary_to_speed_list(Other_Bytes)};
        ?SECTOR_3_SPEED_PACKET ->
            {sector_speed, 3, binary_to_speed_list(Other_Bytes)};
        ?SPEED_TRAP_PACKET ->
            {speed_trap, binary_to_speed_list(Other_Bytes)};
        ?FASTEST_LAP_CAR_PACKET ->
            {fastest_lap_car, binary_to_integer(Other_Bytes)};
        ?FASTEST_LAP_DRIVER_PACKET ->
            {fastest_lap_driver, Other_Bytes};
        ?FASTEST_LAP_TIME_PACKET ->
            {fastest_lap_time, binary_to_lap_time(Other_Bytes)};
        ?FASTEST_LAP_LAP_PACKET ->
            {fastest_lap_lap, binary_to_integer(Other_Bytes)}
    end;

to_system_message(?SESSION_STATUS_PACKET, ?SESSION_FLAG_PACKET, Payload) ->
    Status =
        case binary_to_integer(Payload) of
            ?GREEN_FLAG ->
                green_flag;
            ?YELLOW_FLAG ->
                yellow_flag;
            ?SCS->
                scs;
            ?SCD->
                scd;
            ?RED_FLAG ->
                red_flag
        end,
    {session_status, Status};

to_system_message(?COPYRIGHT_PACKET, _Extra, Payload) ->
    {copyright, Payload};

to_system_message(Type, Extra, Payload) ->
    error_logger:error_msg("Unknown system packet (type ~p, extra ~p, payload ~p)~n",
                           [Type, Extra, Payload]),
    undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec binary_to_gap(binary()) -> gap() | undefined.
binary_to_gap(<<>>) ->
    undefined;

binary_to_gap(<<L:1/bytes, $L>>) ->
    {laps, binary_to_integer(L)};

binary_to_gap(<<L:2/bytes, $L>>) ->
    {laps, binary_to_integer(L)};

binary_to_gap(Binary) ->
    {time, binary_to_lap_time(Binary)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec binary_to_lap_time(binary()) -> in_pit | out | retired | stop |
                                      lap_time() | undefined.
binary_to_lap_time(<<>>) ->
    undefined;

binary_to_lap_time(<<"IN PIT">>) ->
    in_pit;

binary_to_lap_time(<<"OUT">>) ->
    out;

binary_to_lap_time(<<"RETIRED">>) ->
    retired;

binary_to_lap_time(<<"STOP">>) ->
    stop;

binary_to_lap_time(<<M:1/bytes, $:, S:2/bytes, $., Z:3/bytes>>) ->
    %% M:SS.ZZZ format
    {binary_to_integer(M), binary_to_integer(S), binary_to_integer(Z)};

binary_to_lap_time(<<S:2/bytes, $., Z:1/bytes>>) ->
    %% SS.Z format
    {0, binary_to_integer(S), binary_to_integer(Z) * 100};

binary_to_lap_time(<<S:1/bytes, $., Z:1/bytes>>) ->
    %% S.Z format
    {0, binary_to_integer(S), binary_to_integer(Z) * 100}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec binary_to_speed_list(binary()) -> [{binary(), number()}].
binary_to_speed_list(Binary) ->
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
-spec binary_to_time(binary()) -> calendar:time() | undefined.
binary_to_time(<<>>) ->
    undefined;

binary_to_time(<<M:2/bytes, $:, S:2/bytes>>) ->
    %% MM:SS format
    {0, binary_to_integer(M), binary_to_integer(S)};

binary_to_time(<<M:1/bytes, $:, S:2/bytes>>) ->
    %% M:SS format
    {0, binary_to_integer(M), binary_to_integer(S)}.
