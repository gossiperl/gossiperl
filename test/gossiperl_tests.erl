-module(gossiperl_tests).
-include_lib("eunit/include/eunit.hrl").

-include_lib("gossiperl.hrl").

gossiperl_test_() ->
  {setup, fun start/0, fun stop/1, [
    fun test_get_iface_ip/0,
    fun test_get_all_ipv4_addrs/0,
    fun test_get_timestamp/0,
    fun test_parse_binary_ip/0,
    fun test_binary_join/0,
    fun test_configuration_property_from_json/0,
    fun test_rack_configuration/0,
    fun test_valid_configuration_from_json/0,
    fun test_invalid1_configuration_from_json/0,
    fun test_invalid2_configuration_from_json/0,
    fun test_invalid3_configuration_from_json/0,
    fun no_seeds_for_a_rack/0,
    fun configuration_with_member_name/0,
    fun configuration_process/0
    ] }.

-define(OVERLAY, test_overlay).
-define(COMMON, gossiperl_common).
-define(CONFIG, gossiperl_configuration).

start() ->
  Applications = [ asn1, crypto, public_key, erlsha2, jsx, thrift,
                   quickrand, uuid, cowlib, ranch, cowboy,
                   syntax_tools, compiler, goldrush,
                   lager, uuid, gossiperl ],
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

test_configuration_property_from_json() ->
  ConfigRecord = #overlayConfig{},
  ConfigRecord2 = ?CONFIG:configuration_property_from_json( <<"member_name">>,
                                                            <<"my_member_name">>,
                                                            ConfigRecord ),
  ?assertEqual( ConfigRecord2#overlayConfig.member_name, <<"my_member_name">> ),
  ConfigRecord3 = ?CONFIG:configuration_property_from_json( <<"ip">>,
                                                            <<"127.0.0.1">>,
                                                            ConfigRecord2 ),
  ?assertMatch( {127,0,0,1}, ConfigRecord3#overlayConfig.ip ),
  ConfigRecord4 = ?CONFIG:configuration_property_from_json( <<"port">>,
                                                            54321,
                                                            ConfigRecord3 ),
  ?assertEqual( ConfigRecord4#overlayConfig.port, 54321 ),

  ?assertMatch( {error, _}, ?CONFIG:configuration_property_from_json( <<"port">>,
                                                                      <<"54321">>,
                                                                      ConfigRecord4 ) ).

test_rack_configuration() ->
  ConfigRecord = #overlayConfig{},
  ConfigRecord2 = ?CONFIG:configuration_property_from_json( <<"racks">>,
                                                            [{<<"rack1">>, [<<"127.0.0.1">>]}],
                                                            ConfigRecord ),
  ?assertMatch( [{<<"rack1">>, [{127,0,0,1}]}], ConfigRecord2#overlayConfig.racks ),
  InvalidRackSetup = ?CONFIG:configuration_property_from_json( <<"racks">>,
                                                               [{<<"rack1">>, [<<"notip">>]}],
                                                               ConfigRecord2 ),
  ?assertMatch( {error, _}, InvalidRackSetup ).

test_valid_configuration_from_json() ->
  % This is a valid configuration:
  Configuration = <<"{ \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"192.168.50.100\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>,
  JsonData = jsx:decode( Configuration ),
  ?assertMatch({ ok, _ }, ?CONFIG:configuration_from_json(JsonData, ?OVERLAY)).

test_invalid1_configuration_from_json() ->
  % port is not numeric:
  Configuration = <<"{ \"ip\": \"0.0.0.0\", \"port\": \"6666\", \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"192.168.50.100\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>,
  ParseData = jsx:decode( Configuration ),
  ?assertMatch({ error, _ }, ?CONFIG:configuration_from_json(ParseData, ?OVERLAY)).

test_invalid2_configuration_from_json() ->
  % IP address in not an IP address:
  Configuration = <<"{ \"ip\": \"no an ip address\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"192.168.50.100\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>,
  ParseData = jsx:decode( Configuration ),
  ?assertMatch({ error, _ }, ?CONFIG:configuration_from_json(ParseData, ?OVERLAY)).

test_invalid3_configuration_from_json() ->
  % one of the rack IPs is not an IP:
  Configuration = <<"{ \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"not an ip\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>,
  ParseData = jsx:decode( Configuration ),
  ?assertMatch({ error, _ }, ?CONFIG:configuration_from_json(ParseData, ?OVERLAY)).

no_seeds_for_a_rack() ->
  % made up interface name:
  Configuration = <<"{ \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"some_other_rack\": [\"192.168.50.100\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>,
  { ok, ParseConfiguration } = ?CONFIG:configuration_from_json( jsx:decode( Configuration ), ?OVERLAY ),
  ValidationResult = ?CONFIG:validate( ParseConfiguration ),
  ?assertMatch({ error, _ }, ValidationResult).

configuration_with_member_name() ->
  Configuration = <<"{ \"member_name\": \"hello_world\", \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"192.168.50.100\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>,
  JsonData = jsx:decode( Configuration ),
  ParseResult = ?CONFIG:configuration_from_json(JsonData, ?OVERLAY),
  ?assertMatch( { ok,_ }, ParseResult ),
  { ok, ParseConfiguration } = ParseResult,
  SetupConfig = ?CONFIG:setup( ParseConfiguration ),
  ?assertEqual( SetupConfig#overlayConfig.member_name, <<"hello_world">> ).

configuration_process() ->
  Configuration = <<"{ \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"192.168.50.100\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>,
  JsonData = jsx:decode( Configuration ),
  ParseResult = ?CONFIG:configuration_from_json(JsonData, ?OVERLAY),
  ?assertMatch( { ok,_ }, ParseResult ),
  { ok, ParseConfiguration } = ParseResult,
  SetupConfig = ?CONFIG:setup( ParseConfiguration ),
  ?assertEqual( SetupConfig#overlayConfig.name, ParseConfiguration#overlayConfig.name ),
  StoredConfig = ?CONFIG:store_config( SetupConfig ),
  ?assertEqual(SetupConfig, StoredConfig),
  ?assertEqual( 1, length( ?CONFIG:list_overlays() ) ),
  ?assertMatch( { ok, _ }, ?CONFIG:for_overlay( ?OVERLAY ) ),
  ?assertEqual( { error, no_config }, ?CONFIG:for_overlay( non_existing_overlay ) ),
  ?assertEqual( true, ?CONFIG:remove_configuration_for( ?OVERLAY ) ),
  ?assertEqual( 0, length( ?CONFIG:list_overlays() ) ).

% Encryption

% Serialization

