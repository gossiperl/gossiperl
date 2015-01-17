-module(gossiperl_tests).
-include_lib("eunit/include/eunit.hrl").

gossiperl_test_() ->
  {setup, fun start/0, fun stop/1, [
    fun test_get_iface_ip/0,
    fun test_get_all_ipv4_addrs/0,
    fun test_get_timestamp/0,
    fun test_parse_binary_ip/0,
    fun test_binary_join/0
    ] }.

-define(COMMON, gossiperl_common).

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

% Common

test_get_iface_ip() ->
  { ok, [ If | _ ] } = inet:getiflist(),
  BinIf = list_to_binary( If ),
  NotInterface = <<"NotAnInterface">>,
  ?assertMatch({ error, no_iface }, ?COMMON:get_iface_ip( NotInterface )),
  ?assertMatch({ ok, { _,_,_,_ } }, ?COMMON:get_iface_ip( BinIf )).

test_get_all_ipv4_addrs() ->
  ?assertMatch([ _ | _ ], ?COMMON:get_all_ipv4_addrs()).

test_get_timestamp() ->
  ?assertEqual( ?COMMON:get_timestamp( {1421,450191,9061} ), 1421450191 ).

test_parse_binary_ip() ->
  CorrectIp = <<"127.0.0.1">>,
  NotIp = <<"Some random value">>,
  ?assertError(function_clause, ?COMMON:parse_binary_ip( binary_to_list( CorrectIp ) )),
  ?assertMatch({_, _, _, _}, ?COMMON:parse_binary_ip( CorrectIp )),
  ?assertMatch({error, {not_ip, _}}, ?COMMON:parse_binary_ip( NotIp )).

test_binary_join() ->
  Bin1 = <<"A">>,
  Bin2 = <<"B">>,
  Bin3 = <<"C">>,
  ?assertError(undef, ?COMMON:binary_join([ Bin1, Bin2, Bin3 ])),
  ?assertError(undef, ?COMMON:binary_join(123)),
  ?assertEqual( <<"ABC">>, ?COMMON:binary_join([ Bin1, Bin2, Bin3 ], <<>>) ),
  ?assertEqual( <<"A,B,C">>, ?COMMON:binary_join([ Bin1, Bin2, Bin3 ], <<",">>) ).

% Configuration

% Encryption

% Serialization

