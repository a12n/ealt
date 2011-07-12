%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%% Internal protocol revision 0 translations.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_translator_0).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Registers packet translator in event manager.
%%
%% @spec add_handler() -> ok | {'EXIT', Reason :: term()} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    ealt_event_manager:add_handler(?SERVER, []).

%%--------------------------------------------------------------------
%% @doc
%% Unregisters packet translator in event manager.
%%
%% @spec delete_handler() -> term() | {error, module_not_found} | {'EXIT', Reason :: term()}
%% @end
%%--------------------------------------------------------------------
delete_handler() ->
    ealt_event_manager:delete_handler(?SERVER, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args :: term()) -> {ok, State :: #state{}}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({client_connected, Client}, State) ->
    %% TODO: Send current state to the client.
    ealt_dispatcher:dispatch(Client, ealt_messages:format(hello, 0, undefined)),
    {ok, State};
handle_event({packet_extracted, Packet}, State) ->
    case translate_packet(Packet, State) of
        {ok, Tag, Content, State_1} ->
            ealt_dispatcher:dispatch(ealt_messages:format(Tag, 0, Content)),
            {ok, State_1};
        {no_message, State_1} ->
            {ok, State_1}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
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
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
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
%% TODO
%%
%% @spec translate_packet(Packet :: term(), State :: #state{}) ->
%%     {ok, Tag :: atom(), Content :: term(), State_1 :: #state{}} |
%%     {no_message, State_1 :: #state{}}
%% @end
%%--------------------------------------------------------------------
translate_packet(_Packet, State) ->
    {no_message, State}.

%% translate_packet(#packet{car = 0, type = ?SYSTEM_PACKET_COMMENTARY, payload = Payload}, State = #state{commentary_buffer = Commentary_Buffer}) ->
%%     <<_Byte_1, _Byte_2:7, Flush_Bit, Text/bytes>> = Payload,
%%     Commentary_Buffer_1 = <<Commentary_Buffer/bytes, Text/bytes>>,
%%     case Flush_Bit of
%%         0 ->
%%             {noreply, State#state{commentary_buffer = Commentary_Buffer_1}};
%%         1 ->
%%             {ok, commentary, binary_to_list(Commentary_Buffer_1), State#state{commentary_buffer = <<>>}}
%%     end;
%% translate_packet(Packet, State) ->
%%     {ok, raw_packet, Packet, State}.

%% transform_packet(Timestamp, #packet{car = Car, type = ?CAR_PACKET_POSITION_UPDATE, data = Position}) ->
%%     [{car_position, 0, Timestamp, {Car, Position}}];
%% %% System packets
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_EVENT_ID, payload = <<_, Event>>}) ->
%%     [{event, 0, Timestamp, binary_to_list(Event)}];
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_VALID_MARKER, data = Is_Valid}) ->
%%     [{valid, 0, Timestamp, Is_Valid =:= 1}];

%% transform_packet(#packet{car = 0, type = ?SYSTEM_PACKET_COMMENTARY, payload = Payload}, State = #state{commentary = Commentary}) ->
%%     <<Byte_1, Byte_2, Text/bytes>> = Payload,
%%     Commentary_1 = <<Commentary/bytes, Text/bytes>>,
%%     case Byte_2 of
%%         2#00000000 when Byte_1 >= 32 ->
%%             State_1 = State#state{commentary = Commentary_1},
%%             {nomessage, State_1};
%%         2#00000001 when Byte_1 >= 32 ->
%%             Message = ealt_messages:format_message(commentary, 0, {eng, binary_to_list(Commentary_1)}),
%%             State_1 = State#state{commentary = <<>>},
%%             {message, Message, State_1}
%%     end;
%% transform_packet(_Packet, State) ->
%%     ?debugFmt("Unknown packet ~p", [_Packet]),
%%     {nomessage, State}.
            
    %% [{commentary, 0, now(), {eng, binary_to_list(Text)}}].
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_NOTICE, payload = Payload}) ->
%%     [{notice, 0, Timestamp, binary_to_list(Payload)}];
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_TIMESTAMP, data = Byte_1, payload = <<Byte_2, Byte_3>>}) ->
%%     Seconds = (Byte_1 bsl 16) bor Byte_2 bor (Byte_3 bsl 8),
%%     [{timestamp, 0, Timestamp, calendar:seconds_to_time(Seconds)}];
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_WEATHER, data = Sub_Type, payload = Payload}) ->
%%     {Tag, Content} = weather_message_content(Sub_Type, Payload),
%%     [{Tag, 0, Timestamp, Content}];
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_SPEED, payload = <<Type, Rest/bytes>>}) ->
%%     Tag =
%%         case Type of
%%             ?SPEED_PACKET_SECTOR_1 -> sector_1;
%%             ?SPEED_PACKET_SECTOR_2 -> sector_2;
%%             ?SPEED_PACKET_SECTOR_3 -> sector_3;
%%             ?SPEED_PACKET_SPEED_TRAP -> speed_trap;
%%             ?SPEED_PACKET_FASTEST_LAP_CAR -> fastest_lap_car;
%%             ?SPEED_PACKET_FASTEST_LAP_DRIVER -> fastest_lap_driver;
%%             ?SPEED_PACKET_FASTEST_LAP_TIME -> fastest_lap_time;
%%             ?SPEED_PACKET_FASTEST_LAP_LAP -> fastest_lap_lap
%%         end,
%%     [{Tag, 0, Timestamp, Rest}];
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_TRACK_STATUS, data = ?TRACK_STATUS_PACKET_FLAG, payload = Payload}) ->
%%     {Tag, Content} = track_status_message_content(?TRACK_STATUS_PACKET_FLAG, Payload),
%%     [{Tag, 0, Timestamp, Content}];
%% transform_packet(Timestamp, #packet{car = 0, type = ?SYSTEM_PACKET_COPYRIGHT, payload = Payload}) ->
%%     [{copyright, 0, Timestamp, binary_to_list(Payload)}];
%% transform_packet(Timestamp, Packet) ->
%%     [{raw_packet, 0, Timestamp, Packet}].
%%
%%
%% binary_to_integer(Binary) ->
%%     list_to_integer(binary_to_list(Binary)).
%%
%% binary_to_number(Binary) ->
%%     list_to_number(binary_to_list(Binary)).
%%
%% binary_to_time(<<_M:2/bytes, $:, _S:2/bytes>> = Binary) ->
%%     binary_to_time(<<$0, $:, Binary/bytes>>);
%% binary_to_time(<<_M:1/bytes, $:, _S:2/bytes>> = Binary) ->
%%     binary_to_time(<<$0, $:, $0, Binary/bytes>>);
%% binary_to_time(<<H, $:, M_1, M_2, $:, S_1, S_2>>) ->
%%     {H - $0,
%%      M_2 - $0 + (M_1 - $0) * 10,
%%      S_2 - $0 + (S_1 - $0) * 10};
%% binary_to_time(<<$0>>) ->
%%     {0, 0, 0};
%% binary_to_time(<<>>) ->
%%     {0, 0, 0}.
%%
%% list_to_number(List) ->
%%     ?debugFmt("List ~p to number...", [List]),
%%     try
%%         erlang:list_to_float(List)
%%     catch
%%         error : badarg ->
%%             float(list_to_integer(List))
%%     end.
%%
%% track_status_message_content(?TRACK_STATUS_PACKET_FLAG, Flag) ->
%%     case binary_to_integer(Flag) of
%%         ?FLAG_GREEN -> {session_status, green_flag};
%%         ?FLAG_YELLOW -> {session_status, yellow_flag};
%%         ?FLAG_SCS -> {session_status, scs};
%%         ?FLAG_SCD -> {session_status, scd};
%%         ?FLAG_RED -> {session_status, red_flag}
%%     end.
%%
%% weather_message_content(?WEATHER_PACKET_SESSION_CLOCK, Clock) ->
%%     {session_clock, binary_to_time(Clock)};
%% weather_message_content(?WEATHER_PACKET_TRACK_TEMP, Temp) ->
%%     {track_temp, binary_to_number(Temp)};
%% weather_message_content(?WEATHER_PACKET_AIR_TEMP, Temp) ->
%%     {air_temp, binary_to_number(Temp)};
%% weather_message_content(?WEATHER_PACKET_WET_TRACK, Flag) ->
%%     {track_condition, case Flag of "1" -> wet; _ -> dry end};
%% weather_message_content(?WEATHER_PACKET_WIND_SPEED, Speed) ->
%%     {wind_speed, binary_to_number(Speed)}; % m/s
%% weather_message_content(?WEATHER_PACKET_HUMIDITY, Humidity) ->
%%     {relative_humidity, binary_to_number(Humidity)}; % %
%% weather_message_content(?WEATHER_PACKET_PRESSURE, Pressure) ->
%%     {atmospheric_pressure, binary_to_number(Pressure)}; % mBar
%% weather_message_content(?WEATHER_PACKET_WIND_DIRECTION, Direction) ->
%%     {wind_direction, binary_to_number(Direction)}.
