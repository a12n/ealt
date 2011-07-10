%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%% Protocol translator notifications.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_event_manager).

%% API
-export([add_handler/2, client_connected/1, delete_handler/2,
         packet_extracted/1, start/0, start_link/0, stop/0]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds packet handler to the event manager.
%%
%% @spec add_handler(Handler :: atom(), Args :: term()) -> ok | {'EXIT', Reason :: term()} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Notify packet handlers about connected client.
%%
%% @spec client_connected(Client :: port()) -> ok
%% @end
%%--------------------------------------------------------------------
client_connected(Client) ->
    gen_event:notify(?SERVER, {client_connected, Client}).

%%--------------------------------------------------------------------
%% @doc
%% Deletes packet handler from the event manager.
%%
%% @spec delete_handler(Handler :: atom(), Args :: term()) -> term() | {error, module_not_found} | {'EXIT', Reason :: term()}
%% @end
%%--------------------------------------------------------------------
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Notify packet handlers about extracted packet.
%%
%% @spec packet_extracted(Packet :: #packet{}) -> ok
%% @end
%%--------------------------------------------------------------------
packet_extracted(Packet) ->
    gen_event:notify(?SERVER, {packet_extracted, Packet}).

%%--------------------------------------------------------------------
%% @doc
%% Creates stand-alone event manager.
%%
%% @spec start() -> {ok, Pid :: pid()} | {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_event:start({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Creates event manager in a supervision tree.
%%
%% @spec start_link() -> {ok, Pid :: pid()} | {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Stops event manager.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_event:stop(?SERVER).
