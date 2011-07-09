
%% Dispatcher
-define(DEFAULT_DISPATCHER_PORT, 8642).

%% Extractor
-define(LIVE_TIMING_HOST, "live-timing.formula1.com").
-define(LIVE_TIMING_PORT, 4321).

%% Decoder
-define(SECURE_HOST, "secure.formula1.com").

%% For use in HTTP related guards!
-define(is_success(Status_Code), Status_Code >= 200, Status_Code =< 205).
-define(is_redirection(Status_Code), Status_Code >= 300, Status_Code =< 304).
-define(is_success_or_redirection(Status_Code), ?is_success(Status_Code); ?is_redirection(Status_Code)).
