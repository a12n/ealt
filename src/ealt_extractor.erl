%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_extractor).

-include("ealt_macros.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {buffer = <<>>, keyframe_id = undefined, socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(Old_Vsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_Old_Vsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve packet keyframe of specified id. Parameter is a non negative integer
%% <em>Keyframe_Id</em> extracted from keyframe packet.
%%
%% @spec keyframe(Keyframe_Id :: non_neg_integer()) ->
%%     {ok, Bytes :: binary()} |
%%     {error, Reason :: term()} |
%%     {http_error, Reason :: term()}
%% @end
%%--------------------------------------------------------------------
keyframe(Keyframe_Id) ->
    Headers = [],
    HTTP_Options = [],
    Options = [{body_format, binary}],
    case httpc:request(get, {keyframe_url(Keyframe_Id), Headers}, HTTP_Options, Options, ealt) of
        {ok, {{_, Status_Code, _}, _, Content}} when ?is_success(Status_Code) ->
            {ok, Content};
        {ok, {{_, _, Reason}, _, _}} ->
            {http_error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This URL should be used then keyframe of packets should be
%% received. Requires non negative integer <em>Keyframe_Id</em>
%% parameter.
%%
%% @spec keyframe_url(Keyframe_Id :: non_neg_integer()) -> string()
%% @end
%%--------------------------------------------------------------------
keyframe_url(0) ->
    lists:flatten(io_lib:format("http://~s/keyframe.bin", [?LIVE_TIMING_HOST]));
keyframe_url(Keyframe_Id) ->
    lists:flatten(io_lib:format("http://~s/keyframe_~5..0B.bin",
                                [?LIVE_TIMING_HOST, Keyframe_Id])).
