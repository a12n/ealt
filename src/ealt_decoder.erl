%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%% Packet decryption server.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_decoder).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_macros.hrl").
-include("ealt_packets.hrl").

-behaviour(gen_server).

%% API
-export([process_packet/1, start/0, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(INITIAL_MASK, 16#55555555).

-record(state, {key = 0, mask = ?INITIAL_MASK}).

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
    gen_server:cast(?SERVER, {process_packet, Packet}).

%%--------------------------------------------------------------------
%% @doc
%% Starts stand-alone decrypting server.
%%
%% @spec start() ->
%%     {ok, Pid :: pid()} |
%%     ignore |
%%     {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts decrypting server in supervision tree.
%%
%% @spec start_link() ->
%%     {ok, Pid :: pid()} |
%%     ignore |
%%     {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops decrypting server.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

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
init([]) ->
    {ok, #state{}}.

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
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({process_packet, Packet}, State) ->
    {Packet_1, State_1} = decrypt_packet(Packet, State),
    ?debugFmt("Packet ~p decrypted.", [Packet_1]),
    State_2 = handle_special_packet(Packet_1, State_1),
    ealt_event_manager:packet_extracted(Packet_1),
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
decrypt_packet(Packet = #packet{payload = {true, Payload}},
               State = #state{key = Key, mask = Mask}) ->
    {Payload_1, Mask_1} = decrypt_payload(Payload, Key, Mask),
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
%% Decrypt <em>Bytes</em> of payload.
%%
%% @spec decrypt_payload(Encrypted :: binary(), Key :: integer(), Mask :: integer()) ->
%%     {Decrypted :: binary(), Mask_1 :: integer()}
%% @end
%%--------------------------------------------------------------------
decrypt_payload(Encrypted, Key, Mask) ->
    decrypt_payload(<<>>, Encrypted, Key, Mask).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decrypt <em>Bytes</em> of payload appending to the provided binary.
%%
%% @spec decrypt_payload(Decrypted :: binary(),
%%                       Encrypted :: binary(),
%%                       Key :: integer(),
%%                       Mask :: integer()) ->
%%     {Decrypted_1 :: binary(), Mask_1 :: integer()}
%% @end
%%--------------------------------------------------------------------
decrypt_payload(Decrypted, <<>>, _Key, Mask) ->
    {Decrypted, Mask};
decrypt_payload(Decrypted, <<Byte, Encrypted_1/bytes>>, Key, Mask) ->
    Mask_1 = ((Mask bsr 1) bxor (Mask band 1) * Key) band 16#ffffffff,
    Byte_1 = Byte bxor (Mask_1 band 16#ff),
    Decrypted_1 = <<Decrypted/bytes, Byte_1>>,
    decrypt_payload(Decrypted_1, Encrypted_1, Key, Mask_1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles event id packet. Retrieves appropriate key and updates
%% <em>State</em>.
%%
%% @spec handle_special_packet(Packet :: #packet{}, State :: #state{}) ->
%%     State_1 :: #state{}
%% @end
%%--------------------------------------------------------------------
handle_special_packet(Packet = #packet{car_id = 0,
                                       type = ?SYSTEM_PACKET_EVENT_ID},
                      State) ->
    Event_Id = ealt_packets:convert_payload(Packet),
    {ok, Cookie} = ealt_auth:user_cookie(),
    {ok, Key} = key(Event_Id, Cookie),
    ?debugFmt("Received key ~p.", [Key]),
    State#state{key = Key, mask = ?INITIAL_MASK};
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
