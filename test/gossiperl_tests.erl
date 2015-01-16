-module(gossiperl_tests).
-include_lib("eunit/include/eunit.hrl").

gossiperl_test_() ->
  {setup, fun start/0, fun stop/1, [] }.

start() ->
  Applications = [ asn1, crypto, public_key, erlsha2, jsx, thrift,
                   quickrand, uuid, cowlib, ranch, cowboy,
                   syntax_tools, compiler, goldrush,
                   lager, gossiperl ],
  [ begin
      Result = application:start(App),
      error_logger:info_msg("Starting application ~p: ~p", [ App, Result ] )
    end || App <- Applications ],
  ok.

stop(_State) ->
  noreply.