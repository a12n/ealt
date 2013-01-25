%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_translator).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-import(erlang, [make_tuple/2]).

%% #state.messages is a proplist. Key is either system message atom
%% (e.g. wet_track, wind_direction, etc.) or pair of car message atom
%% (e.g. n_pits, position, etc.) and car id. Value is JSON message
%% formatted as binary.  #state.fastest_lap accumulates fastest lap
%% information (car, driver, time and lap), and then all information
%% is available a single JSON message is issued for clients.
-record(state, { messages    = [],
                 fastest_lap = make_tuple(4, undefined) }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Registers packet translator in event manager.
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    ealt_events:add_handler(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Unregisters packet translator in event manager.
%% @end
%%--------------------------------------------------------------------
delete_handler() ->
    ealt_events:delete_handler(?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%% @end
%%--------------------------------------------------------------------
handle_event({client_connected, Client}, State = #state{ messages = Messages }) ->
    lists:foreach(fun({_Key, Message}) -> Client(Message) end, Messages),
    {ok, State};

handle_event({message_decoded, Message}, State) ->
    {Formatted, Next_State} =
        handle_message(Message, State),
    case Formatted of
        undefined ->
            ok;
        _Other ->
            ?debugVal(Formatted),
            ealt_websocket:dispatch(Formatted)
    end,
    {ok, Next_State};

handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    {ok, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed.
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
%% @end
%%--------------------------------------------------------------------
-spec handle_message(ealt_message:message(), #state{}) ->
                            {binary() | undefined, #state{}}.

%% Handle fastest lap system messages. Accumulate data and when all
%% parts are available, inject fake fastest_lap message.
handle_message({FL_Type, Datum}, State = #state{ fastest_lap = FL })
  when FL_Type =:= fastest_lap_car;
       FL_Type =:= fastest_lap_driver;
       FL_Type =:= fastest_lap_lap;
       FL_Type =:= fastest_lap_time ->
    Idx_Fun = fun(fastest_lap_car)    -> 1;
                 (fastest_lap_driver) -> 2;
                 (fastest_lap_lap)    -> 3;
                 (fastest_lap_time)   -> 4 end,
    Next_FL = {Car, Driver, Lap, Time} =
        setelement(Idx_Fun(FL_Type), FL, Datum),
    if (Car =/= undefined) and
       (Driver =/= undefined) and
       (Lap =/= undefined) and
       (Time =/= undefined) ->
            Message = {fastest_lap, Car, Driver, Lap, Time},
            Next_State = State#state{ fastest_lap = make_tuple(4, undefined) },
            handle_message(Message, Next_State);
       true ->
            Next_State = State#state{ fastest_lap = Next_FL },
            {undefined, Next_State}
    end;

handle_message(Message, State = #state{ messages = Messages }) ->
    {Key, Formatted} = format_message(Message),
    Next_Messages =
        case Key of
            undefined ->
                Messages;
            _Other_Key ->
                lists:keystore(Key, 1, Messages, {Key, Formatted})
        end,
    Next_State =
        State#state{ messages = Next_Messages },
    {Formatted, Next_State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_message(ealt_message:message()) ->
                            {term() | undefined, binary()}.

format_message(Message) ->
    try
        {Key, Fields} = message_properties(Message),
        {M, S, U} = os:timestamp(),
        Timestamp = (M * 1.0E+6) + S + (U * 1.0E-6),
        Type = atom_to_binary(element(1, Message), latin1),
        Formatted = jsx:to_json([ {type, Type},
                                  {timestamp, Timestamp} | Fields ]),
        {Key, Formatted}
    catch
        error : _Error ->
            {undefined, undefined}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message_properties(ealt_message:message()) ->
                                {term() | undefined, [{atom(), term()}]}.

%% Modifying state system messages
message_properties({Type = air_temperature, Temp}) ->
    {Type, [ {temp, Temp} ]};

message_properties({Type = atmospheric_pressure, Pressure}) ->
    {Type, [ {pressure, Pressure} ]};

message_properties({Type = copyright, Text}) ->
    {Type, [ {text, Text} ]};

message_properties({Type = event, Id, Session}) ->
    {Type, [ {id, Id},
             {session, atom_to_binary(Session, latin1)} ]};

message_properties({Type = relative_humidity, Humidity}) ->
    {Type, [ {humidity, Humidity} ]};

message_properties({Type = sector_speed, Sector, Table}) ->
    {Type, [ {sector, Sector},
             {table, Table} ]};

message_properties({Type = session_clock, Time}) ->
    {Type, [ {time, [ {h, element(1, Time)},
                      {min, element(2, Time)},
                      {s, element(3, Time)} ]} ]};

message_properties({Type = session_status, Status}) ->
    {Type, [ {status, atom_to_binary(Status, latin1)} ]};

message_properties({Type = speed_trap, Table}) ->
    {Type, [ {table, Table} ]};

message_properties({Type = track_temperature, Temp}) ->
    {Type, [ {temp, Temp} ]};

message_properties({Type = wet_track, Wet}) ->
    {Type, [ {wet, Wet} ]};

message_properties({Type = wind_direction, Direction}) ->
    {Type, [ {direction, Direction} ]};

message_properties({Type = wind_speed, Speed}) ->
    {Type, [ {speed, Speed} ]};

message_properties({Type = fastest_lap, Car, Driver, Lap, Time}) ->
    {Type, [ {car, Car},
             {driver, Driver},
             {lap, Lap},
             {time, lap_time_properties(Time)} ]};

%% Non-modifying state system messages
message_properties({commentary, Flush, Text}) ->
    {undefined, [ {flush, Flush},
                  {text, Text} ]};

message_properties({notice, Text}) ->
    {undefined, [ {text, Text} ]};

%% Modifying state car messages
message_properties({Type = best_time, Car, Time}) ->
    {{Type, Car}, [ {car, Car},
                    {time, lap_time_properties(Time)} ]};

message_properties({Type = driver, Car, Name}) ->
    {{Type, Car}, [ {car, Car},
                    {name, Name} ]};

message_properties({Type = gap, Car, Gap}) ->
    {{Type, Car}, [ {car, Car},
                    {gap, gap_properties(Gap)} ]};

message_properties({Type = interval, Car, Interval}) ->
    {{Type, Car}, [ {car, Car},
                    {interval, gap_properties(Interval)} ]};

message_properties({Type = lap, Car, Lap}) ->
    {{Type, Car}, [ {car, Car},
                    {lap, Lap} ]};

message_properties({Type = n_pits, Car, N}) ->
    {{Type, Car}, [ {car, Car},
                    {n, N} ]};

message_properties({Type = number, Car, Num}) ->
    {{Type, Car}, [ {car, Car},
                    {num, Num} ]};

message_properties({Type = period_time, Car, Period, Time}) ->
    {{Type, Car}, [ {car, Car},
                    {period, Period},
                    {time, lap_time_properties(Time)} ]};

message_properties({Type, Car, Pos})
  when Type =:= position;
       Type =:= position_history;
       Type =:= position_update ->
    {{Type, Car}, [ {car, Car},
                    {pos, Pos} ]};

message_properties({Type = sector_time, Car, Sector, Time}) ->
    {{Type, Car}, [ {car, Car},
                    {sector, Sector},
                    {time, lap_time_properties(Time)} ]};

message_properties({Type, Car})
  when Type =:= in_pit;
       Type =:= out;
       Type =:= retired;
       Type =:= stop ->
    {{Type, Car}, [ {car, Car} ]};

%% Non-modifying state car messages
message_properties({lap_time, Car, Time}) ->
    {undefined, [ {car, Car},
                  {time, lap_time_properties(Time)} ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lap_time_properties(ealt_message:lap_time()) ->
                                 [{atom(), term()}].

lap_time_properties({M, S, L}) ->
    [ {min, M},
      {s, S},
      {ms, L} ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec gap_properties(ealt_message:gap()) ->
                            [{atom(), term()}].

gap_properties({laps, N}) ->
    [ {laps, N} ];

gap_properties({time, T}) ->
    [ {time, lap_time_properties(T)} ].
