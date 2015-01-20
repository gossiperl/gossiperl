-module(gossiperl_rest_api_tests).
-include_lib("eunit/include/eunit.hrl").

-include_lib("gossiperl_tests.hrl").

gossiperl_rest_api_test_() ->
  {setup, fun start/0, fun stop/1, [
    fun dummy/0
    ] }.

start() ->
  [ begin
      Result = application:start(App),
      error_logger:info_msg("Starting application ~p: ~p", [ App, Result ] )
    end || App <- ?APPLICATIONS ],
  ok.

stop(_State) ->
  [ application:stop(App) || App <- ?APPLICATIONS ],
  noreply.

dummy() ->
  ?assertEqual(1,1).