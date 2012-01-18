%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% `ealt' application behaviour implementation module.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the live timing application. Supervisor and protocol
%% translators are started.
%% @end
%%--------------------------------------------------------------------
-spec start(term(), term()) -> {ok, pid()} | {error, term()}.
start(_Start_Type, _Start_Args) ->
    ealt_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops live timing application.
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> void().
stop(_State) ->
    ok.
