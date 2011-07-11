
-record(packet, {car_id = 0, type, data = 0, payload = <<>>, encrypted = false}).

%% Car packet types.
-define(CAR_PACKET_POSITION_UPDATE, 0).
%% Types from 1 to 13 are update packets for live timing display columns.
-define(CAR_PACKET_POSITION_HISTORY, 15).

%% System packet types.
-define(SYSTEM_PACKET_EVENT_ID, 1).
-define(SYSTEM_PACKET_KEYFRAME, 2).
-define(SYSTEM_PACKET_VALID_MARKER, 3).
-define(SYSTEM_PACKET_COMMENTARY, 4).
-define(SYSTEM_PACKET_REFRESH_RATE, 5).
-define(SYSTEM_PACKET_NOTICE, 6).
-define(SYSTEM_PACKET_TIMESTAMP, 7).
-define(SYSTEM_PACKET_WEATHER, 9).
-define(SYSTEM_PACKET_SPEED, 10).
-define(SYSTEM_PACKET_TRACK_STATUS, 11).
-define(SYSTEM_PACKET_COPYRIGHT, 12).

%% Sub-types of the SYSTEM_PACKET_WEATHER.
-define(WEATHER_PACKET_SESSION_CLOCK, 0).
-define(WEATHER_PACKET_TRACK_TEMP, 1).
-define(WEATHER_PACKET_AIR_TEMP, 2).
-define(WEATHER_PACKET_WET_TRACK, 3).
-define(WEATHER_PACKET_WIND_SPEED, 4).
-define(WEATHER_PACKET_HUMIDITY, 5).
-define(WEATHER_PACKET_PRESSURE, 6).
-define(WEATHER_PACKET_WIND_DIRECTION, 7).

%% Sub-types of the SYSTEM_PACKET_SPEED (first byte of payload).
-define(SPEED_PACKET_SECTOR_1, 1).
-define(SPEED_PACKET_SECTOR_2, 2).
-define(SPEED_PACKET_SECTOR_3, 3).
-define(SPEED_PACKET_SPEED_TRAP, 4).
-define(SPEED_PACKET_FASTEST_LAP_CAR, 5).
-define(SPEED_PACKET_FASTEST_LAP_DRIVER, 6).
-define(SPEED_PACKET_FASTEST_LAP_TIME, 7).
-define(SPEED_PACKET_FASTEST_LAP_LAP, 8).

%% Sub-types of the SYSTEM_PACKET_TRACK_STATUS.
-define(TRACK_STATUS_PACKET_FLAG, 1).

%% Payload values for TRACK_STATUS_PACKET_FLAG.
-define(FLAG_GREEN, 1).
-define(FLAG_YELLOW, 2).
-define(FLAG_SAFETY_CAR_STAND_BY, 3).
-define(FLAG_SAFETY_CAR_DEPLOYED, 4).
-define(FLAG_RED, 5).
-define(FLAG_SCS, ?FLAG_SAFETY_CAR_STAND_BY).
-define(FLAG_SCD, ?FLAG_SAFETY_CAR_DEPLOYED).
