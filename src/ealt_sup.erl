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

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor.
%%
%% @spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
%%
%% @spec init(Args :: term()) -> {ok, {Sup_Flags, [Child_Spec]}} |
%%                              ignore |
%%                              {error, Reason :: term()}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Restart_Strategy = one_for_one,
    Max_Restarts = 1,
    Max_Seconds_Between_Restarts = 10,
    Sup_Flags = {Restart_Strategy, Max_Restarts, Max_Seconds_Between_Restarts},
    %% Event manager
    Event_Manager = {ealt_event_manager, {ealt_event_manager, start_link, []},
                     permanent, 5000, worker, [ealt_event_manager]},
    %% Internal protocol dispatcher
    Dispatcher = {ealt_dispatcher, {ealt_dispatcher, start_link, []},
                  permanent, 5000, worker, [ealt_dispatcher]},
    %% External protocol extractor and decoder
    Decoder = {ealt_decoder, {ealt_decoder, start_link, []},
               permanent, 5000, worker, [ealt_decoder]},
    Extractor = {ealt_extractor, {ealt_extractor, start_link, []},
                 permanent, 5000, worker, [ealt_extractor]},
    {ok, {Sup_Flags, [Event_Manager, Dispatcher, Decoder, Extractor]}}.
