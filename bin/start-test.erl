#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ../ebin/

main([Email, Password]) ->
    ok = inets:start(),
    {ok, _Pid_1} = inets:start(httpc, [{profile, ealt}]),
    ok = ssl:start(),
    ok = httpc:set_option(cookies, enabled, ealt),
    ok = ealt_auth:login(Email, Password),
    {ok, _Pid_5} = ealt_event_manager:start(),
    {ok, _Pid_6} = ealt_dispatcher:start(),
    {ok, _Pid_7} = ealt_decoder:start(),
    {ok, _Pid_8} = ealt_extractor:start(),
    timer:sleep(infinity).
