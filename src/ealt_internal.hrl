
-define(LIVE_TIMING_HOST, "live-timing.formula1.com").
-define(LIVE_TIMING_PORT, 4321).
-define(SECURE_HOST, "secure.formula1.com").

-define(dump_value(Value), error_logger:info_msg("~p:~p, ~p = ~p~n", [?MODULE, ?LINE, ??Value, Value])).

%% For use in HTTP related guards.
-define(is_success(Status), Status >= 200, Status =< 205).
-define(is_redirection(Status), Status >= 300, Status =< 304).
-define(is_success_or_redirection(Status), ?is_success(Status); ?is_redirection(Status)).
