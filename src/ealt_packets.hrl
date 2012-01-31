
-type payload() :: {plain | scrambled, binary()}.

-record(packet, { car     :: integer(),
                  type    :: integer(),
                  extra   :: integer(),
                  payload :: payload() }).

%%----------------------------------------------------------------------------
%% Car packet types.

-define(POSITION_UPDATE_PACKET, 0).
%% Types from 1 to 13 are update packets for live timing display columns.
-define(RACE_POSITION_PACKET, 1).
-define(RACE_NUMBER_PACKET, 2).
-define(RACE_DRIVER_PACKET, 3).
-define(RACE_GAP_PACKET, 4).
-define(RACE_INTERVAL_PACKET, 5).
-define(RACE_LAP_TIME_PACKET, 6).
-define(RACE_SECTOR_1_TIME_PACKET, 7).
-define(RACE_PIT_LAP_1_PACKET, 8).
-define(RACE_SECTOR_2_TIME_PACKET, 9).
-define(RACE_PIT_LAP_2_PACKET, 10).
-define(RACE_SECTOR_3_TIME_PACKET, 11).
-define(RACE_PIT_LAP_3_PACKET, 12).
-define(RACE_N_PITS_PACKET, 13).
-define(PRACTICE_POSITION_PACKET, 1).
-define(PRACTICE_NUMBER_PACKET, 2).
-define(PRACTICE_DRIVER_PACKET, 3).
-define(PRACTICE_BEST_TIME_PACKET, 4).
-define(PRACTICE_GAP_PACKET, 5).
-define(PRACTICE_SECTOR_1_TIME_PACKET, 6).
-define(PRACTICE_SECTOR_2_TIME_PACKET, 7).
-define(PRACTICE_SECTOR_3_TIME_PACKET, 8).
-define(PRACTICE_LAP_PACKET, 9).
-define(QUALIFYING_POSITION_PACKET, 1).
-define(QUALIFYING_NUMBER_PACKET, 2).
-define(QUALIFYING_DRIVER_PACKET, 3).
-define(QUALIFYING_PERIOD_1_PACKET, 4).
-define(QUALIFYING_PERIOD_2_PACKET, 5).
-define(QUALIFYING_PERIOD_3_PACKET, 6).
-define(QUALIFYING_SECTOR_1_TIME_PACKET, 7).
-define(QUALIFYING_SECTOR_2_TIME_PACKET, 8).
-define(QUALIFYING_SECTOR_3_TIME_PACKET, 9).
-define(QUALIFYING_LAP_PACKET_PACKET, 10).
-define(POSITION_HISTORY_PACKET, 15).

%%----------------------------------------------------------------------------
%% System packet types.

-define(EVENT_PACKET, 1).
-define(KEYFRAME_PACKET, 2).
-define(MARKER_PACKET, 3).
-define(COMMENTARY_PACKET, 4).
-define(REFRESH_TIME_PACKET, 5).
-define(NOTICE_PACKET, 6).
-define(TIMESTAMP_PACKET, 7).
-define(WEATHER_PACKET, 9).
-define(SPEED_PACKET, 10).
-define(SESSION_STATUS_PACKET, 11).
-define(COPYRIGHT_PACKET, 12).

%% Sub-types of the EVENT_PACKET.
-define(RACE_EVENT, 1).
-define(PRACTICE_EVENT, 2).
-define(QUALIFYING_EVENT, 3).

%% Sub-types of the WEATHER_PACKET.
-define(SESSION_CLOCK_PACKET, 0).
-define(TRACK_TEMPERATURE_PACKET, 1).
-define(AIR_TEMPERATURE_PACKET, 2).
-define(WET_TRACK_PACKET, 3).
-define(WIND_SPEED_PACKET, 4).
-define(RELATIVE_HUMIDITY_PACKET, 5).
-define(ATMOSPHERIC_PRESSURE_PACKET, 6).
-define(WIND_DIRECTION_PACKET, 7).

%% Sub-types of the SPEED_PACKET (first byte of payload).
-define(SECTOR_1_SPEED_PACKET, 1).
-define(SECTOR_2_SPEED_PACKET, 2).
-define(SECTOR_3_SPEED_PACKET, 3).
-define(SPEED_TRAP_PACKET, 4).
-define(FASTEST_LAP_CAR_PACKET, 5).
-define(FASTEST_LAP_DRIVER_PACKET, 6).
-define(FASTEST_LAP_TIME_PACKET, 7).
-define(FASTEST_LAP_LAP_PACKET, 8).

%% Sub-types of the SESSION_STATUS_PACKET.
-define(SESSION_FLAG_PACKET, 1).

%% Payload values for SESSION_FLAG_PACKET.
-define(GREEN_FLAG, 1).
-define(YELLOW_FLAG, 2).
-define(SAFETY_CAR_STAND_BY, 3).
-define(SAFETY_CAR_DEPLOYED, 4).
-define(RED_FLAG, 5).
