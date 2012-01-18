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
%% Starts the live timing application. Supervisor and protocol translators are started.
%%
%% @spec start(Start_Type, Start_Args) ->
%%     {ok, Pid :: pid()} |
%%     {error, Reason :: term()}
%% @end
%%--------------------------------------------------------------------
start(_Start_Type, _Start_Args) ->
    {ok, Email} = application:get_env(ealt, email),
    {ok, Password} = application:get_env(ealt, password),
    ok = httpc:set_option(cookies, enabled, ealt),
    ok = ealt_auth:login(Email, Password),
    case ealt_sup:start_link() of
        Result = {ok, _Pid} ->
            ok = ealt_translator_0:add_handler(),
            Result;
        Result = {error, _Reason} ->
            Result
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops live timing application.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
