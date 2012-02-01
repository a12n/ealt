%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Root supervisor of the `ealt' application.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}.
init(_Args) ->
    %% Event manager
    Events = {ealt_events, {ealt_events, start_link, []},
              permanent, 5000, worker, [ealt_events]},

    %% WebSocket server
    WebSocket_Address = ealt_app:get_env(websocket_address, {127, 0, 0, 1}),
    WebSocket_Port = ealt_app:get_env(websocket_port, 8642),
    WebSocket_Path = ealt_app:get_env(websocket_path, [<<"events">>]),
    Dispatch = [{'_', [{WebSocket_Path, ealt_websocket, []}]}],
    WebSocket =
        cowboy:child_spec(ealt_websocket, 10,
                          cowboy_tcp_transport, [{ip, WebSocket_Address},
                                                 {port, WebSocket_Port}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]),

    %% External protocol extractor and decoder
    %% Decoder = {ealt_decoder, {ealt_decoder, start_link, []},
    %%            permanent, 5000, worker, [ealt_decoder]},

    Flags = {one_for_one, 2, 10},
    {ok, {Flags, [Events, WebSocket]}}.
