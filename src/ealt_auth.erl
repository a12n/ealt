%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Authentication related functions.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_auth).

%% API
-export([login/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Authenticates to the live timing service. Retrieves and stores authentication
%% cookies. Requires <em>Email</em> and <em>Password</em> of some valid site
%% account. Returns `{ok, Cookie}' on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec login(binary(), binary()) -> {ok, string()} | {error, term()} |
                                   {http_error, term()}.
login(Email, Password) ->
    URL = "http://www.formula1.com/reg/login",
    Headers = [],
    Content_Type = "application/x-www-form-urlencoded",
    Content = cowboy_http:urlencoded(<<"email=", Email/bytes,
                                       "&password=", Password/bytes>>),
    HTTP_Options = [{autoredirect, false}],
    Options = [{body_format, binary}],
    case httpc:request(post, {URL, Headers, Content_Type, Content}, HTTP_Options, Options, ealt) of
        {ok, {{_, Status, _}, _, _}} when Status =:= 200 ->
            {error, unathorized};
        %% Redirects to live timing host on successful authentication.
        {ok, {{_, Status, _}, _, _}} when Status =:= 302 ->
            {ok, cookie()};
        {ok, {{_, _, Reason}, _, _}} ->
            {http_error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Searches list of HTTP cookies for "USER" cookie of ".formula1.com" domain.
%% @end
%%--------------------------------------------------------------------
-spec cookie() -> string().
cookie() ->
    All_Cookies = httpc:which_cookies(ealt),
    {cookies, Cookies} = lists:keyfind(cookies, 1, All_Cookies),
    {http_cookie, ".formula1.com", _Domain_Default, _Name, Value,
     _Comment, _Max_Age, _Path, _Path_Default, _Secure, _Version} =
        lists:keyfind("USER", 4, Cookies),
    Value.
