%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_decoder).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(ealt_utils, [bounded/3]).

-include("ealt.hrl").

-define(MIN_REFRESH_TIME, 1000).
-define(MAX_REFRESH_TIME, 120000).

-define(PING_PACKET, <<16>>).

-define(INITIAL_MASK, 16#55555555).

-record(state, { buffer = <<>>,
                 key = 0,
                 keyframe_id,
                 mask = ?INITIAL_MASK,
                 refresh_time = ?MIN_REFRESH_TIME,
                 session,
                 socket }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts packet extraction server in supervision tree.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, #state{}, integer()}.
init(_Args) ->
    Options = [binary, {packet, 0}],
    {ok, Socket} = gen_tcp:connect(?LIVE_TIMING_HOST, ?LIVE_TIMING_PORT, Options),
    State = #state{ socket = Socket },
    {ok, State, State#state.refresh_time}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Bytes}, State = #state{ buffer = Buffer }) ->
    Next_State = read_packets(State#state{ buffer = <<Buffer/bytes, Bytes/bytes>> }),
    {noreply, Next_State, Next_State#state.refresh_time};
handle_info({tcp_closed, _Socket}, State) ->
    error_logger:error_msg("Server closed connection~n"),
    {stop, tcp_closed, State};
handle_info({tcp_error, Reason}, State) ->
    error_logger:error_msg("Server connection error, reason ~p~n", [Reason]),
    {stop, tcp_error, State};
handle_info(timeout, State = #state{ refresh_time = Timeout, socket = Socket }) ->
    ok = gen_tcp:send(Socket, ?PING_PACKET),
    {noreply, State, Timeout}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
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
%% Handles refresh rate, keyframe and event packet terms. Modifies
%% <em>State</em> accordingly.
%% @end
%%--------------------------------------------------------------------
-spec handle_special_packet(term(), #state{}) -> #state{}.
handle_special_packet({event, Id, Session}, State) ->
    {ok, Key} = key(Id, ealt_auth:cookie()),
    State#state{ key = Key, mask = ?INITIAL_MASK, session = Session };
handle_special_packet({keyframe, Next_Id}, State = #state{ buffer = Buffer,
                                                           keyframe_id = Id }) ->
    %% TODO: Reset mask on new keyframe?
    case Id of
        undefined ->
            {ok, Keyframe_Bytes} = keyframe(Next_Id),
            State#state{ buffer = <<Keyframe_Bytes/bytes, Buffer/bytes>>,
                         keyframe_id = Next_Id };
        _Other ->
            State#state{ keyframe_id = Next_Id }
    end;
handle_special_packet({refresh_time, Time}, State) ->
    Next_Refresh_Time =
        bounded(Time * 1000, ?MIN_REFRESH_TIME, ?MAX_REFRESH_TIME),
    State#state{ refresh_time = Next_Refresh_Time };
handle_special_packet(_Term, State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve encryption key for the specific <em>Event_Id</em>. Requires
%% authentication <em>Cookie</em>.
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), binary()) -> {ok, integer()} | {error, term()} |
                                 {http_error, term()}.
key(Event_Id, Cookie) ->
    case httpc:request(key_url(Event_Id, Cookie), ealt) of
        {ok, {{_, Status, _}, _, Content}} when Status =:= 200 ->
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
%% @end
%%--------------------------------------------------------------------
-spec key_url(binary(), binary()) -> string().
key_url(Event_Id, Cookie) ->
    lists:flatten(io_lib:format("http://~s/reg/getkey/~s.asp?auth=~s",
                                [?LIVE_TIMING_HOST, Event_Id, Cookie])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve packet keyframe of specified id. Parameter is a non
%% negative integer <em>Keyframe_Id</em> extracted from keyframe
%% packet.
%% @end
%%--------------------------------------------------------------------
-spec keyframe(non_neg_integer()) -> {ok, binary()} | {error, term()} |
                                    {http_error, term()}.
keyframe(Keyframe_Id) ->
    Headers = [],
    HTTP_Options = [],
    Options = [{body_format, binary}],
    case httpc:request(get, {keyframe_url(Keyframe_Id), Headers}, HTTP_Options, Options, ealt) of
        {ok, {{_, Status, _}, _, Content}} when Status =:= 200 ->
            {ok, Content};
        {ok, {{_, _, Reason}, _, _}} ->
            {http_error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This URL should be used then keyframe of packets should be
%% received. Requires non negative integer <em>Keyframe_Id</em>
%% parameter.
%%
%% @end
%%--------------------------------------------------------------------
-spec keyframe_url(non_neg_integer()) -> string().
keyframe_url(0) ->
    lists:flatten(io_lib:format("http://~s/keyframe.bin", [?LIVE_TIMING_HOST]));
keyframe_url(Keyframe_Id) ->
    lists:flatten(io_lib:format("http://~s/keyframe_~5..0B.bin",
                                [?LIVE_TIMING_HOST, Keyframe_Id])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses key representation <em>Str</em> as integer key.
%% @end
%%--------------------------------------------------------------------
-spec parse_key(string()) -> {ok, integer()} | {error, term()}.
parse_key(Str) ->
    try
        {ok, list_to_integer(Str, 16)}
    catch
        error : badarg ->
            {error, invalid_key}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract packets from buffer while there are any.
%% @end
%%--------------------------------------------------------------------
-spec read_packets(#state{}) -> #state{}.
read_packets(State = #state{ buffer = Buffer,
                             key = Key,
                             mask = Mask,
                             session = Session }) ->
    case ealt_packets:read_packet(Buffer) of
        {ok, Scrambled_Packet, Next_Buffer} ->
            {Packet, Next_Mask} =
                ealt_packets:descramble_packet(Scrambled_Packet, Key,
                                               Mask),
            Packet_Term =
                ealt_packets:packet_to_term(Session, Packet),
            ?dump_value({Scrambled_Packet, Packet, Packet_Term}),
            %% TODO: Do not dispatch packet term if it's undefined.
            ealt_events:packet_extracted(Packet_Term),
            Next_State =
                handle_special_packet(Packet_Term,
                                      State#state{ buffer = Next_Buffer,
                                                   mask = Next_Mask }),
            read_packets(Next_State);
        {more, _Length} ->
            State
    end.
