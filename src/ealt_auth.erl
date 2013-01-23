%%%-------------------------------------------------------------------
%%% @copyright 2012, Anton Yabchinskiy <arn@users.berlios.de>
%%% @doc
%%% Authentication server. Logs into the live timging server, renews
%%% authentication cookie.
%%% @end
%%% @todo Add docs and specs.
%%% @todo Renew cookie after _Max_Age. _Max_Age is gregorian seconds
%%% of expiration date.
%%%-------------------------------------------------------------------
-module(ealt_auth).

-behaviour(gen_server).

%% API
-export([cookie/0, start/2, start_link/2, stop/0]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Authentication cookie.
%% @end
%%--------------------------------------------------------------------
-spec cookie() -> binary().
cookie() ->
    gen_server:call(?MODULE, cookie).

%%--------------------------------------------------------------------
%% @doc
%% Starts standalone authentication server. Parameters are
%% <em>Email</em> and <em>Password</em>.
%% @end
%%--------------------------------------------------------------------
-spec start(binary(), binary()) -> {ok, pid()} | ignore |
                                   {error, term()}.
start(Email, Password) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Email, Password}, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts supervised authentication server. Parameters are
%% <em>Email</em> and <em>Password</em>.
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary(), binary()) -> {ok, pid()} | ignore |
                                        {error, term()}.
start_link(Email, Password) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Email, Password}, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops standalone authentication server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
code_change(_Old_Vsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_call(cookie, _From, State = Cookie) ->
    {reply, Cookie, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init({binary(), binary()}) -> {ok, term()} | {stop, term()}.
init({Email, Password}) ->
    URL = "http://www.formula1.com/reg/login",
    Headers = [],
    Content_Type = "application/x-www-form-urlencoded",
    Encoded_Email = cowboy_http:urlencode(Email),
    Encoded_Password = cowboy_http:urlencode(Password),
    Content = <<"email=", Encoded_Email/bytes,
                "&password=", Encoded_Password/bytes>>,
    HTTP_Options = [{autoredirect, false}],
    Options = [],
    case httpc:request(post, {URL, Headers, Content_Type, Content},
                       HTTP_Options, Options, ealt) of
        %% Redirects to live timing host on successful authentication.
        {ok, {{_, Status, _}, _, _}} when Status =:= 302 ->
            find_cookie();
        {ok, {{_, Status, _}, _, _}} when Status =:= 200 ->
            {stop, unathorized};
        {ok, {{_, _, Reason}, _, _}} ->
            {stop, Reason};
        {error, Reason} ->
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Searches the list of HTTP cookies for "USER" cookie of
%% ".formula1.com" domain.
%% @end
%%--------------------------------------------------------------------
-spec find_cookie() -> {ok, binary()} | {stop, term()}.
find_cookie() ->
    case lists:keyfind(cookies, 1, httpc:which_cookies(ealt)) of
        {cookies, Cookies} ->
            case lists:keyfind("USER", 4, Cookies) of
                {http_cookie, ".formula1.com", _Domain_Default, _Name,
                 Value, _Comment, _Max_Age, _Path, _Path_Default,
                 _Secure, _Version} ->
                    {ok, list_to_binary(Value)};
                _Other ->
                    {stop, no_cookie}
            end;
        _Other ->
            {stop, no_cookies}
    end.
