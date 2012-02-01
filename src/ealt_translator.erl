%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Internal protocol revision 0 translations. Just sends internal
%%% representation of packets.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_translator).

-behaviour(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Registers packet translator in event manager.
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    ealt_event_manager:add_handler(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Unregisters packet translator in event manager.
%% @end
%%--------------------------------------------------------------------
delete_handler() ->
    ealt_event_manager:delete_handler(?MODULE, []).

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
handle_event({client_connected, Client}, State) ->
    %% TODO: Send current state to the client.
    {ok, State};
handle_event({packet_extracted, Packet}, State) ->
    %% TODO: Translate and send.
    {ok, State};
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
