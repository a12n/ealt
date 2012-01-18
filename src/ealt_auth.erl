%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Authentication related functions.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_auth).

-include_lib("eunit/include/eunit.hrl").

-include("ealt_macros.hrl").

%% API
-export([login/2, user_cookie/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Authenticates to the live timing service. Retrieves and stores authentication
%% cookies. Requires <em>Email</em> and <em>Password</em> of some valid site
%% account.
%%
%% @spec login(Email :: string(), Password :: string()) ->
%%     ok |
%%     {error, Reason :: term()} |
%%     {http_error, Reason :: term()}
%% @end
%%--------------------------------------------------------------------
login(Email, Password) ->
    ?debugFmt("Email \"~s\", password \"~s\".", [Email, Password]),
    Headers = [],
    Content_Type = "application/x-www-form-urlencoded",
    Content = urlencoded([{email, Email}, {password, Password}]),
    HTTP_Options = [{autoredirect, false}],
    Options = [{body_format, binary}],
    case httpc:request(post, {login_url(), Headers, Content_Type, Content}, HTTP_Options, Options, ealt) of
        {ok, {{_, Status_Code, _}, _, _}} when ?is_success(Status_Code) ->
            ?debugMsg("Invalid credentials."),
            {error, unathorized};
        %% Redirects to live timing host on successful authentication.
        {ok, {{_, Status_Code, _}, _, _}} when ?is_redirection(Status_Code) ->
            ?debugMsg("Authenticated."),
            ok;
        {ok, {{_, _, Reason}, _, _}} ->
            ?debugFmt("HTTP error, reason ~p.", [Reason]),
            {http_error, Reason};
        {error, Reason} ->
            ?debugFmt("Error, reason ~p.", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Searches list of retrieved cookies for user's authentication cookie.
%%
%% @spec user_cookie() -> {ok, Value :: string()} | undefined
%% @end
%%--------------------------------------------------------------------
user_cookie() ->
    search_cookie_stores(httpc:which_cookies(ealt)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% URL which should be used to post credentials and retrieve authentication
%% cookies.
%%
%% @spec login_url() -> string()
%% @end
%%--------------------------------------------------------------------
login_url() ->
    lists:flatten(io_lib:format("https://~s/reg/login", [?SECURE_HOST])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Searches "USER" cookie in every of cookie stores.
%% 
%% @spec search_cookie_stores(Cookie_Stores :: [cookie_stores()]) ->
%%     {ok, Value :: string()} | undefined
%% @end
%%--------------------------------------------------------------------
search_cookie_stores([{cookies, Cookies} | Other]) ->
    ?debugMsg("Searching in cookies..."),
    case search_cookies(Cookies) of
        Result = {ok, _Value} ->
            Result;
        undefined ->
            search_cookie_stores(Other)
    end;
search_cookie_stores([{session_cookies, Session_Cookies} | Other]) ->
    ?debugMsg("Searching in session cookies..."),
    case search_cookies(Session_Cookies) of
        Result = {ok, _Value} ->
            Result;
        undefined ->
            search_cookie_stores(Other)
    end;
search_cookie_stores([]) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Searches list of HTTP cookies for "USER" cookie of ".formula1.com" domain.
%% 
%% @spec search_cookies(Cookies :: [icookie()]) -> {ok, Value :: string()} | undefined
%% @end
%%--------------------------------------------------------------------
search_cookies([{http_cookie, ".formula1.com", _Domain_Default, "USER", Value,
                 _Comment, _Max_Age, _Path, _Path_Default, _Secure, _Version} | _Other]) ->
    ?debugFmt("User cookie found, value \"~s\"", [Value]),
    {ok, Value};
search_cookies([_Cookie | Other]) ->
    search_cookies(Other);
search_cookies([]) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% URL encodes it's input. Accepts atoms, strings, and property lists as the
%% parameter.
%% 
%% @todo Replace with distinct functions?
%%
%% @spec urlencoded(term()) -> string()
%% @end
%%--------------------------------------------------------------------
urlencoded(Properties = [{_Key, _Value} | _Other]) -> %% Proplist case.
    string:join(lists:map(fun urlencoded/1, Properties), "&");
urlencoded({Key, Value}) -> %% Property case.
    lists:append([urlencoded(Key), "=", urlencoded(Value)]);
urlencoded(Atom) when is_atom(Atom) -> %% Atom case.
    urlencoded(atom_to_list(Atom));
urlencoded([$; | Rest]) -> [$%, $3, $B | urlencoded(Rest)]; %% String cases.
urlencoded([$/ | Rest]) -> [$%, $2, $F | urlencoded(Rest)];
urlencoded([$? | Rest]) -> [$%, $3, $F | urlencoded(Rest)];
urlencoded([$: | Rest]) -> [$%, $3, $A | urlencoded(Rest)];
urlencoded([$@ | Rest]) -> [$%, $4, $0 | urlencoded(Rest)];
urlencoded([$& | Rest]) -> [$%, $2, $6 | urlencoded(Rest)];
urlencoded([$= | Rest]) -> [$%, $3, $D | urlencoded(Rest)];
urlencoded([$+ | Rest]) -> [$%, $2, $B | urlencoded(Rest)];
urlencoded([$$ | Rest]) -> [$%, $2, $4 | urlencoded(Rest)];
urlencoded([$, | Rest]) -> [$%, $2, $C | urlencoded(Rest)];
urlencoded([$  | Rest]) -> [$+ | urlencoded(Rest)];
urlencoded([Char | Rest]) -> [Char | urlencoded(Rest)];
urlencoded([]) -> [].
