%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Authentication related functions.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_auth).

-include("ealt.hrl").

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
-spec login(string(), string()) -> {ok, string()} | {error, term()} |
                                  {http_error, term()}.
login(Email, Password) ->
    ?dump_value({Email, Password}),
    Headers = [],
    Content_Type = "application/x-www-form-urlencoded",
    Content = lists:concat(["email=", urlencoded(Email), "&password=", urlencoded(Password)]),
    HTTP_Options = [{autoredirect, false}],
    Options = [{body_format, binary}],
    case httpc:request(post, {login_url(), Headers, Content_Type, Content}, HTTP_Options, Options, ealt) of
        {ok, {{_, Status_Code, _}, _, _}} when ?is_success(Status_Code) ->
            {error, unathorized};
        %% Redirects to live timing host on successful authentication.
        {ok, {{_, Status_Code, _}, _, _}} when ?is_redirection(Status_Code) ->
            search_cookie();
        {ok, {{_, _, Reason}, _, _}} ->
            ?dump_value({http_error, Reason}),
            {http_error, Reason};
        {error, Reason} ->
            ?dump_value({error, Reason}),
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% URL which should be used to post credentials and retrieve authentication
%% cookies.
%% @end
%%--------------------------------------------------------------------
-spec login_url() -> string().
login_url() ->
    lists:flatten(io_lib:format("https://~s/reg/login", [?SECURE_HOST])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Searches list of HTTP cookies for "USER" cookie of ".formula1.com" domain.
%% @end
%%--------------------------------------------------------------------
-spec search_cookie() -> {ok, string()}.
search_cookie() ->
    All_Cookies = httpc:which_cookies(ealt),
    {cookies, Cookies} = lists:keyfind(cookies, 1, All_Cookies),
    {http_cookie, ".formula1.com", _Domain_Default, _Name, Value,
     _Comment, _Max_Age, _Path, _Path_Default, _Secure, _Version} =
        lists:keyfind("USER", 4, Cookies),
    {ok, Value}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% URL encode string.
%% @end
%%--------------------------------------------------------------------
-spec urlencoded(string()) -> string().
urlencoded([$; | Other]) -> [$%, $3, $B | urlencoded(Other)];
urlencoded([$/ | Other]) -> [$%, $2, $F | urlencoded(Other)];
urlencoded([$? | Other]) -> [$%, $3, $F | urlencoded(Other)];
urlencoded([$: | Other]) -> [$%, $3, $A | urlencoded(Other)];
urlencoded([$@ | Other]) -> [$%, $4, $0 | urlencoded(Other)];
urlencoded([$& | Other]) -> [$%, $2, $6 | urlencoded(Other)];
urlencoded([$= | Other]) -> [$%, $3, $D | urlencoded(Other)];
urlencoded([$+ | Other]) -> [$%, $2, $B | urlencoded(Other)];
urlencoded([$$ | Other]) -> [$%, $2, $4 | urlencoded(Other)];
urlencoded([$, | Other]) -> [$%, $2, $C | urlencoded(Other)];
urlencoded([$  | Other]) -> [$+ | urlencoded(Other)];
urlencoded([Char | Other]) -> [Char | urlencoded(Other)];
urlencoded([]) -> [].
