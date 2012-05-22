%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% TODO
%%% @end
%%%-------------------------------------------------------------------
-module(ealt).

%% API
-export([start/0, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    start_deps(),
    application:start(ealt).

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
stop() ->
    Result = application:stop(ealt),
    stop_deps(),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> ok.
start_deps() ->
    ensure_started(cowboy),
    %% ensure_started(crypto),
    %% ensure_started(public_key),
    %% ensure_started(ssl),
    ensure_started(inets),
    ok = httpc:set_option(cookies, enabled, ealt).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec stop_deps() -> ok.
stop_deps() ->
    application:stop(inets),
    %% application:stop(ssl),
    %% application:stop(public_key),
    %% application:stop(crypto),
    application:stop(cowboy),
    ok.
