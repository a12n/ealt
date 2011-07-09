%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@users.berlios.de>
%%% @copyright 2011, Anton Yabchinskiy
%%% @doc
%%% Module related to messages of internal protocol. Message is a
%%% Erlang tuple formatted as a line of text like this:
%%% ```{Tag,Version,Timestamp,Content}.\r\n'''
%%%
%%% <em>Tag</em> is an atom identifying the message. E.g. `air_temp',
%%% `commentary', `wind_speed' etc.
%%%
%%% <em>Version</em> is an integer version of specific message. It
%%% starts from 0 and increases when message's content format is altered.
%%%
%%% <em>Timestamp</em> is Erlang timestamp as returned from {@link
%%% erlang:now/0}. It is not timestamp of the actual event, but timestamp of
%%% event registration in the system. It is present mainly for event
%%% stream recording and playback. Has compact in memory representations
%%% (no bignums), could be easily translated on client side to some
%%% native format.
%%%
%%% <em>Content</em> is tag and version dependant payload of the message.
%%%
%%% Client must ignore messages with unknown tag or/and version. Any
%%% extra whitespaces must be ignored. When message's content format
%%% is altered and version increased, message of the previous format is
%%% still sent along with the new message. For example, if there was a
%%% `commentary' message and later it was altered to include language
%%% code, it will be sent to clients this way:
%%% ```
%%% {commentary,0,{1309,957780,256660},"Cars 5 and 6 crashed!111"}.\r\n
%%% {commentary,1,{1309,957780,256662},{eng,"Cars 5 and 6 crashed!111"}}.\r\n
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(ealt_messages).

%% API
-export([format/3, format/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Formats message with `now()' timestamp.
%%
%% @spec format(Tag :: atom(), Vsn :: integer(), Content :: term()) -> iolist()
%% @end
%%--------------------------------------------------------------------
format(Tag, Vsn, Content) ->
    format(Tag, Vsn, now(), Content).

%%--------------------------------------------------------------------
%% @doc
%% Formats message.
%%
%% @spec format(Tag :: atom(), Vsn :: integer(), Timestamp :: {integer(), integer(), integer()}, Content :: term()) -> iolist()
%% @end
%%--------------------------------------------------------------------
format(Tag, Vsn, Timestamp, Content) ->
    io_lib:format("~32768p.\r\n", [{Tag, Vsn, Timestamp, Content}]).
