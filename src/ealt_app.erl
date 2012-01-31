%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% `ealt' application behaviour implementation module.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_app).

-behaviour(application).

%% API
-export([get_env/1, get_env/2]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec get_env(atom()) -> term().
get_env(Key) ->
    {ok, Value} = application:get_env(ealt, Key),
    Value.

%%--------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%--------------------------------------------------------------------
-spec get_env(atom(), term()) -> term().
get_env(Key, Default) ->
    case application:get_env(ealt, Key) of
        {ok, Value} ->
            Value;
        _Other ->
            Default
    end.

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
-spec stop(term()) -> any().
stop(_State) ->
    ok.
