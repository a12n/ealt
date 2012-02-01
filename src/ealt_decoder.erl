%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Packet decryption server.
%%% @end
%%% @todo This module should be merged with extractor server.
%%%-------------------------------------------------------------------
-module(ealt_decoder).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_packets.hrl").
-include("ealt_internal.hrl").

-behaviour(gen_server).

%% API
-export([process_packet/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(INITIAL_MASK, 16#55555555).

-record(state, { cookie, key = 0, mask = ?INITIAL_MASK, session }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Decrypts packet payload, notifies event manager about decrypted packet.
%%
%% @spec process_packet(Packet :: #packet{}) -> ok
%% @end
%%--------------------------------------------------------------------
process_packet(Packet) ->
    gen_server:cast(?MODULE, {process_packet, Packet}).

%%--------------------------------------------------------------------
%% @doc
%% Starts decrypting server in supervision tree.
%% @end
%%--------------------------------------------------------------------
start_link(Cookie) ->
    gen_server:start_link(?MODULE, Cookie, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Cookie) ->
    {ok, #state{ cookie = Cookie }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({process_packet, Packet},
            State = #state{session = Session}) ->
    {Packet_1, State_1} = decrypt_packet(Packet, State),
    ?debugFmt("Packet ~p decrypted.", [Packet_1]),
    Packet_2 = ealt_packets:packet_to_term(Session, Packet_1),
    ?debugFmt("Packet ~p converted.", [Packet_2]),
    State_2 = handle_special_packet(Packet_2, State_1),
    ealt_event_manager:packet_extracted(Packet_2),
    {noreply, State_2};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
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
%% Decrypts packet's payload and updates decryption key in state.
%%
%% @spec decrypt_packet(Packet :: #packet{}, State :: #state{}) ->
%%     {Packet_1 :: #packet{}, State_1 :: #state{}}
%% @end
%%--------------------------------------------------------------------
decrypt_packet(Packet = #packet{payload = {scrambled, Payload}},
               State = #state{key = Key, mask = Mask}) ->
    {Payload_1, Mask_1} = ealt_packets:descramble_payload(Payload, Key, Mask),
    ?debugFmt("Encrypted payload ~p (~p bytes), mask ~p.",
              [Payload, size(Payload), Mask]),
    ?debugFmt("Decrypted payload ~p (~p bytes), mask ~p.",
              [Payload_1, size(Payload_1), Mask_1]),
    Packet_1 = Packet#packet{payload = {false, Payload_1}},
    State_1 = State#state{mask = Mask_1},
    {Packet_1, State_1};
decrypt_packet(Packet, State) ->
    {Packet, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles event id packet. Retrieves appropriate key and updates
%% <em>State</em>. This function expects packets converted to internal
%% representation.
%%
%% @spec handle_special_packet(Packet :: term(), State :: #state{}) ->
%%     State_1 :: #state{}
%% @end
%%--------------------------------------------------------------------
handle_special_packet({event, Event_Id, Session}, State = #state{ cookie = Cookie }) ->
    {ok, Key} = key(Event_Id, Cookie),
    ?debugFmt("Received key ~p.", [Key]),
    State#state{key = Key, mask = ?INITIAL_MASK, session = Session};
handle_special_packet(_Packet, State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve encryption key for the specific <em>Event_Id</em>. Requires
%% authentication <em>Cookie</em> as the second parameter.
%%
%% @spec key(Event_Id :: string(), Cookie :: string()) ->
%%     {ok, Key :: integer()} |
%%     {error, Reason :: term()} |
%%     {http_error, Reason :: term()}
%% @end
%%--------------------------------------------------------------------
key(Event_Id, Cookie) ->
    ?debugFmt("Receiving key for event ~p.", [Event_Id]),
    case httpc:request(key_url(Event_Id, Cookie), ealt) of
        {ok, {{_, Status_Code, _}, _, Content}} when ?is_success(Status_Code) ->
            parse_key(Content);
        {ok, {{_, _, Reason}, _, _}} ->
            {http_error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% URL to retrieve event encryption key. Requires <em>Event_Id</em> and
%% authentication <em>Cookie</em> as the parameters.
%%
%% @spec key_url(Event_Id :: string(), Cookie :: string()) -> string()
%% @end
%%--------------------------------------------------------------------
key_url(Event_Id, Cookie) ->
    lists:flatten(io_lib:format("http://~s/reg/getkey/~s.asp?auth=~s",
                                [?LIVE_TIMING_HOST, Event_Id, Cookie])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses key representation <em>Str</em> as integer key.
%%
%% @spec parse_key(Str :: string()) ->
%%     {ok, Key :: integer()} |
%%     {error, Reason :: term()}
%% @end
%%--------------------------------------------------------------------
parse_key(Str) ->
    ?debugFmt("Parsing key ~p.", [Str]),
    case io_lib:fread("~16u", Str) of
        {ok, [Key], []} ->
            {ok, Key};
        _ ->
            {error, invalid_key}
    end.
