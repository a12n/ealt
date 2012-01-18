%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Miscellaneous utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_utils).

%% API
-export([zip1/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% "Zips" list of items into list of pairs. Length of input list must be
%% even. E.g.:
%% ```
%% 1> zip1([a,b,c,d]).
%% [{a,b},{c,d}]
%% '''
%%
%% @spec zip1(List :: list()) -> [{term(), term()}]
%% @end
%%--------------------------------------------------------------------
-spec zip1([term()]) -> [{term(), term()}].
zip1([Item_1, Item_2 | Other_Items]) ->
    [{Item_1, Item_2} | zip1(Other_Items)];
zip1([]) ->
    [].
