%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% TODO
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_websocket).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

%% API
-export([create_pg/0, dispatch/1]).

%% cowboy_http_handler callbacks
-export([handle/2, init/3, terminate/2]).

%% cowboy_http_websocket_handler callbacks
-export([websocket_handle/3, websocket_info/3, websocket_init/3,
         websocket_terminate/3]).

-define(PG, ealt_websocket_pg).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec create_pg() -> ok | {error, term()}.
create_pg() ->
    pg:create(?PG).

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec dispatch(iodata()) -> ok.
dispatch(Message) ->
    pg:send(?PG, Message).

%%%===================================================================
%%% cowboy_http_handler callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
init({_Transport, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% cowboy_http_websocket_handler callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
websocket_info({pg_message, _From, ?PG, Message}, Req, State) ->
    {reply, {text, Message}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
websocket_init(_Transport, Req, _Opts) ->
    Next_Req = cowboy_http_req:compact(Req),
    _Members = pg:join(?PG, self()),
    ealt_events:client_connected(make_callback()),
    {ok, Next_Req, undefined}.

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates callback function for packet translator. Packet translator
%% may use this callback to inform newly connected client about the
%% current state of the session.
%% @end
%%--------------------------------------------------------------------
-spec make_callback() -> fun().
make_callback() ->
    Self = self(),
    fun(Message) ->
            Self ! {pg_message, self(), ?PG, Message},
            ok
    end.
