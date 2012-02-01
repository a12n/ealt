%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright See LICENSE file.
%%% @doc
%%% Miscellaneous utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_utils).

%% API
-export([binary_to_integer/1, binary_to_number/1, bounded/3, zip1/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts <em>Binary</em> to integer. If conversion is not possible,
%% returns `undefined'.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_integer(binary()) -> integer() | undefined.
binary_to_integer(Binary) ->
    try
        list_to_integer(binary_to_list(Binary))
    catch
        error : badarg ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% Converts <em>Binary</em> to number. If conversion is not possible,
%% returns `undefined'.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_number(binary()) -> number() | undefined.
binary_to_number(Binary) ->
    List = binary_to_list(Binary),
    try
        list_to_integer(List)
    catch
        error : badarg ->
            try
                list_to_float(List)
            catch
                error : badarg ->
                    undefined
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Bounded <em>Value</em>. If <em>Value</em> is less than
%% <em>Min</em>, then <em>Min</em> is returned. If <em>Value</em> is
%% greater than <em>Max</em>, then <em>Max</em> is the
%% result. Otherwise <em>Value</em> itself is returned.
%% @end
%%--------------------------------------------------------------------
-spec bounded(number(), number(), number()) -> number().
bounded(Value, Min, _Max) when Value < Min -> Min;
bounded(Value, _Min, Max) when Value > Max -> Max;
bounded(Value, _Min, _Max) -> Value.

%%--------------------------------------------------------------------
%% @doc
%% "Zips" list of items into list of pairs. Length of input list must be
%% even. E.g.:
%% ```
%% 1> zip1([a,b,c,d]).
%% [{a,b},{c,d}]
%% '''
%% @end
%%--------------------------------------------------------------------
-spec zip1([term()]) -> [{term(), term()}].
zip1([Item_1, Item_2 | Other_Items]) ->
    [{Item_1, Item_2} | zip1(Other_Items)];
zip1([]) ->
    [].
