%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%% Packet payload conversion.
%%%
%%% Timestamp packet has <em>data</em> in `value' field and two bytes of
%%% <em>payload</em>. It could be translated to number of seconds:
%%% ```
%%% <<Byte_1, Byte_2>> = Payload,
%%% Seconds = (Extra bsl 16) bor Byte_1 bor (Byte_2 bsl 8).
%%% '''
%%%
%%% First two bytes of commentary packet payload has special
%%% meaning. If value of the first byte is greater than 31, than the
%%% entire payload is a commentary in ISO-8859-1 character set. If bit two of
%%% the second byte is set, than commentary is in UTF-16-LE, else it is
%%% UTF-8. Finally, if bit one of the second byte is set, than commentary should
%%% be flushed to the output.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_packets).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_packets.hrl").

%% API
-export([convert_packet/1, convert_payload/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convert packet's payload to Erlang term.
%%
%% @spec convert_packet(Packet :: #packet{}) -> #packet{}
%% @end
%%--------------------------------------------------------------------
convert_packet(Packet) ->
    Packet#packet{payload = convert_payload(Packet)}.

%%--------------------------------------------------------------------
%% @doc
%% Convert packet's payload to Erlang term.
%%
%% @spec convert_payload(Packet :: #packet{}) -> term()
%% @end
%%--------------------------------------------------------------------
convert_payload(#packet{car_id = 0,
                        type = ?SYSTEM_PACKET_KEYFRAME,
                        payload = {false, Bytes}}) ->
    <<Keyframe_Id:16/little>> = Bytes,
    Keyframe_Id;
convert_payload(_Packet = #packet{payload = {false, Bytes}}) ->
    ?debugFmt("Couldn't convert payload of packet ~p.", [_Packet]),
    Bytes.
