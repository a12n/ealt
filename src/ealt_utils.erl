%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
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
%% "Zips" list of items into list of two-tuples. Length of input list must be
%% even. E.g.:
%% ```
%% 1> zip1([a,b,c,d]).
%% [{a,b},{c,d}]
%% '''
%%
%% @spec zip1(List :: list()) -> [{term(), term()}]
%% @end
%%--------------------------------------------------------------------
zip1(List) ->
    zip1([], List).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% "Zips" list of items into list of two-tuples. Resulting list is accumulated
%% into the first parameter. Length of input list must be even.
%%
%% @spec zip1(Result :: list(), List :: list()) -> [{term(), term()}]
%% @end
%%--------------------------------------------------------------------
zip1(Result, [Item_1, Item_2 | Other_Items]) ->
    zip1([{Item_1, Item_2} | Result], Other_Items);
zip1(Result, []) ->
    lists:reverse(Result).
