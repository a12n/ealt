%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_translator).

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
%% parts are available, inject fake fastest_lap car message.
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
    {Formatted, Msg_Key} =
        case element(1, Message) of
            %% Modifying state system messages
            SM_Type when SM_Type =:= air_temperature;
                         SM_Type =:= atmospheric_pressure;
                         SM_Type =:= copyright;
                         SM_Type =:= event;
                         SM_Type =:= relative_humidity;
                         SM_Type =:= sector_speed;
                         SM_Type =:= session_clock;
                         SM_Type =:= session_status;
                         SM_Type =:= speed_trap;
                         SM_Type =:= track_temperature;
                         SM_Type =:= wet_track;
                         SM_Type =:= wind_direction;
                         SM_Type =:= wind_speed;
                         %%
                         SM_Type =:= fastest_lap ->
                {format_message(Message), SM_Type};
            %% Non-modifying state system messages
            SN_Type when SN_Type =:= commentary;
                         SN_Type =:= notice ->
                {format_message(Message), undefined};
            %% Modifying state car messages
            CM_Type when CM_Type =:= best_time;
                         CM_Type =:= driver;
                         CM_Type =:= gap;
                         CM_Type =:= interval;
                         CM_Type =:= lap;
                         CM_Type =:= n_pits;
                         CM_Type =:= number;
                         CM_Type =:= period_time;
                         CM_Type =:= position;
                         CM_Type =:= position_history;
                         CM_Type =:= position_update;
                         CM_Type =:= sector_time;
                         %%
                         CM_Type =:= in_pit;
                         CM_Type =:= out;
                         CM_Type =:= retired;
                         CM_Type =:= stop ->
                Car = element(2, Message),
                {format_message(Message), {CM_Type, Car}};
            %% Non-modifying state car messages
            CN_Type when CN_Type =:= lap_time ->
                {format_message(Message), undefined};
            %% Ignore other messages
            _Other_Type ->
                {undefined, undefined}
        end,
    Next_Messages =
        case Msg_Key of
            undefined ->
                Messages;
            _Other_Key ->
                lists:keystore(Msg_Key, 1, Messages, {Msg_Key, Formatted})
        end,
    Next_State =
        State#state{ messages = Next_Messages },
    {Formatted, Next_State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_message(ealt_message:message()) -> binary().

format_message(_Any) ->
    %% TODO
    <<>>.
