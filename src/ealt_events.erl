%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Protocol translator notifications.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_events).

%% API
-export([add_handler/2, client_connected/1, delete_handler/2,
         packet_extracted/1, start_link/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds packet handler to the event manager.
%% @end
%%--------------------------------------------------------------------
-spec add_handler(atom(), term()) -> ok | {'EXIT', term()} | term().
add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Notify packet handlers about connected client.
%% @end
%%--------------------------------------------------------------------
-spec client_connected(fun()) -> ok.
client_connected(Client) ->
    gen_event:notify(?MODULE, {client_connected, Client}).

%%--------------------------------------------------------------------
%% @doc
%% Deletes packet handler from the event manager.
%% @end
%%--------------------------------------------------------------------
-spec delete_handler(atom(), term()) ->
                            term() | {error, module_not_found} |
                            {'EXIT', term()}.
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Notify packet handlers about extracted packet.
%% @end
%%--------------------------------------------------------------------
-spec packet_extracted(term()) -> ok.
packet_extracted(Packet) ->
    gen_event:notify(?MODULE, {packet_extracted, Packet}).

%%--------------------------------------------------------------------
%% @doc
%% Creates event manager in a supervision tree.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link().
