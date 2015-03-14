-module(gossiperl_tests).
-include_lib("eunit/include/eunit.hrl").

-include_lib("gossiperl.hrl").
-include_lib("gossiperl_tests.hrl").

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
    fun test_valid_multicast_configuration_from_json/0,
    fun test_valid_multicast_no_racks_configuration_from_json/0,
    fun test_invalid1_configuration_from_json/0,
    fun test_invalid2_configuration_from_json/0,
    fun test_invalid3_configuration_from_json/0,
    fun test_invalid_multicast_configuration_from_json_1/0,
    fun no_seeds_for_a_rack/0,
    fun configuration_with_member_name/0,
    fun configuration_process/0,
    fun test_serialize_deserialize/0,
    fun test_add_remove_overlay/0,
    fun test_add_remove_multicast_overlay/0,
    fun test_encrypt_decrypt/0
    ] }.

-define(OVERLAY, test_overlay).
-define(COMMON, gossiperl_common).
-define(CONFIG, gossiperl_configuration).

-define(DIGEST_NAME, <<"client-name">>).
-define(DIGEST_PORT, 6666).
-define(DIGEST_HEARTBEAT, 1421450191).
-define(DIGEST_ID, <<"digest-id">>).
-define(DIGEST_SECRET, <<"digest-secret">>).

start() ->
  [ begin
      Result = application:start(App),
      error_logger:info_msg("Starting application ~p: ~p", [ App, Result ] )
    end || App <- ?APPLICATIONS ],
  ok.

stop(_State) ->
  [ application:stop(App) || App <- ?APPLICATIONS ],
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
  Configuration = ?BIN_CONFIG_VALID,
  JsonData = jsx:decode( Configuration ),
  ?assertMatch({ ok, _ }, ?CONFIG:configuration_from_json(JsonData, ?OVERLAY)).

test_valid_multicast_configuration_from_json() ->
  % This is a valid configuration:
  Configuration = ?BIN_CONFIG_VALID_MULTICAST,
  JsonData = jsx:decode( Configuration ),
  ?assertMatch({ ok, _ }, ?CONFIG:configuration_from_json(JsonData, ?OVERLAY)).

test_valid_multicast_no_racks_configuration_from_json() ->
  % This is a valid configuration:
  Configuration = ?BIN_CONFIG_VALID_MULTICAST_NO_RACKS,
  JsonData = jsx:decode( Configuration ),
  ParseResult = ?CONFIG:configuration_from_json(JsonData, ?OVERLAY),
  ?assertMatch({ ok, _ }, ParseResult),
  { ok, ParsedConfiguration } = ParseResult,
  ?assertMatch({ ok, _ }, ?CONFIG:validate_racks(ParsedConfiguration)).

test_invalid1_configuration_from_json() ->
  % port is not numeric:
  Configuration = ?BIN_CONFIG_INVALID1,
  ParseData = jsx:decode( Configuration ),
  ?assertMatch({ error, _ }, ?CONFIG:configuration_from_json(ParseData, ?OVERLAY)).

test_invalid2_configuration_from_json() ->
  % IP address in not an IP address:
  Configuration = ?BIN_CONFIG_INVALID2,
  ParseData = jsx:decode( Configuration ),
  ?assertMatch({ error, _ }, ?CONFIG:configuration_from_json(ParseData, ?OVERLAY)).

test_invalid3_configuration_from_json() ->
  % one of the rack IPs is not an IP:
  Configuration = ?BIN_CONFIG_INVALID3,
  ParseData = jsx:decode( Configuration ),
  ?assertMatch({ error, _ }, ?CONFIG:configuration_from_json(ParseData, ?OVERLAY)).

test_invalid_multicast_configuration_from_json_1() ->
  % one of the rack IPs is not an IP:
  Configuration = ?BIN_CONFIG_INVALID_MULTICAST1,
  ParseData = jsx:decode( Configuration ),
  ?assertMatch({ error, _ }, ?CONFIG:configuration_from_json(ParseData, ?OVERLAY)).

no_seeds_for_a_rack() ->
  % made up interface name:
  Configuration = ?BIN_CONFIG_MISSING_SEEDS,
  { ok, ParseConfiguration } = ?CONFIG:configuration_from_json( jsx:decode( Configuration ), ?OVERLAY ),
  ValidationResult = ?CONFIG:validate_racks( ParseConfiguration ),
  ?assertMatch({ error, _ }, ValidationResult).

configuration_with_member_name() ->
  Configuration = ?BIN_CONFIG_MEMBER_NAME,
  JsonData = jsx:decode( Configuration ),
  ParseResult = ?CONFIG:configuration_from_json(JsonData, ?OVERLAY),
  ?assertMatch( { ok,_ }, ParseResult ),
  { ok, ParseConfiguration } = ParseResult,
  SetupConfig = ?CONFIG:setup( ParseConfiguration ),
  ?assertEqual( SetupConfig#overlayConfig.member_name, <<"hello_world">> ).

configuration_process() ->
  Configuration = ?BIN_CONFIG_VALID,
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

% Serialization
test_serialize_deserialize() ->
  Digest = #digest{ name      = ?DIGEST_NAME,
                    port      = ?DIGEST_PORT,
                    heartbeat = ?DIGEST_HEARTBEAT,
                    id        = ?DIGEST_ID,
                    secret    = ?DIGEST_SECRET },
  SerializeResult = gen_server:call( gossiperl_serialization, { serialize, digest, Digest } ),
  ?assertMatch( { ok, _, _ }, SerializeResult ),
  { ok, SerializedData, _ } = SerializeResult,
  DeserializeResult = gen_server:call( gossiperl_serialization, { deserialize, SerializedData } ),
  ?assertMatch( { ok, _, _ }, DeserializeResult ),
  { ok, PayloadType, Deserialized } = DeserializeResult,
  ?assertEqual( digest, PayloadType ),
  ?assertMatch( { digest, _, _, _, _, _ }, Deserialized ),
  ?assertEqual( ?DIGEST_NAME, Deserialized#digest.name ),
  ?assertEqual( ?DIGEST_PORT, Deserialized#digest.port ),
  ?assertEqual( ?DIGEST_HEARTBEAT, Deserialized#digest.heartbeat ),
  ?assertEqual( ?DIGEST_ID, Deserialized#digest.id ),
  ?assertEqual( ?DIGEST_SECRET, Deserialized#digest.secret ).

% Add / remove overlay basics:
test_add_remove_overlay() ->
  Configuration = ?BIN_CONFIG_VALID,
  JsonData = jsx:decode( Configuration ),
  ParseResult = ?CONFIG:configuration_from_json(JsonData, ?OVERLAY),
  { ok, ParseConfiguration } = ParseResult,
  ?assertMatch( { ok, _ }, gossiperl_sup:add_overlay( ?OVERLAY, ParseConfiguration ) ),
  ?assertEqual( [ list_to_binary(atom_to_list(?OVERLAY)) ], gossiperl_sup:list_overlays() ),
  LoadConfiguratonResult = begin timer:sleep(1000), ?CONFIG:for_overlay( ?OVERLAY ) end,
  ?assertMatch( { ok, { _, _ } }, LoadConfiguratonResult ),
  { ok, { _, LoadedConfiguration } } = LoadConfiguratonResult,
  ?assertEqual( true, gossiperl_sup:remove_overlay( ?OVERLAY, LoadedConfiguration ) ),
  ?assertEqual( [], gossiperl_sup:list_overlays() ).

% Add / remove multicast overlay basics:
test_add_remove_multicast_overlay() ->
  Configuration = ?BIN_CONFIG_VALID_MULTICAST,
  JsonData = jsx:decode( Configuration ),
  ParseResult = ?CONFIG:configuration_from_json(JsonData, ?OVERLAY),
  { ok, ParseConfiguration } = ParseResult,
  ?assertMatch( { ok, _ }, gossiperl_sup:add_overlay( ?OVERLAY, ParseConfiguration ) ),
  ?assertEqual( [ list_to_binary(atom_to_list(?OVERLAY)) ], gossiperl_sup:list_overlays() ),
  LoadConfiguratonResult = begin timer:sleep(1000), ?CONFIG:for_overlay( ?OVERLAY ) end,
  ?assertMatch( { ok, { _, _ } }, LoadConfiguratonResult ),
  { ok, { _, LoadedConfiguration } } = LoadConfiguratonResult,
  ?assertEqual( true, is_port( LoadedConfiguration#overlayConfig.internal#internalConfig.socket ) ),
  ?assertEqual( true, is_port( LoadedConfiguration#overlayConfig.internal#internalConfig.local_socket ) ),
  ?assertEqual( true, gossiperl_sup:remove_overlay( ?OVERLAY, LoadedConfiguration ) ),
  ?assertEqual( [], gossiperl_sup:list_overlays() ).

% Encryption
test_encrypt_decrypt() ->
  Configuration = ?BIN_CONFIG_VALID,
  JsonData = jsx:decode( Configuration ),
  { ok, ParseConfiguration } = ?CONFIG:configuration_from_json(JsonData, ?OVERLAY),
  
  % add overlay:
  gossiperl_sup:add_overlay( ?OVERLAY, ParseConfiguration ),
  
  % give it time to start, it's async, load configuration:
  LoadConfiguratonResult = begin timer:sleep(1000), ?CONFIG:for_overlay( ?OVERLAY ) end,
  { ok, { _, LoadedConfiguration } } = LoadConfiguratonResult,

  % create a digest
  Digest = #digest{ name      = ?DIGEST_NAME,
                    port      = ?DIGEST_PORT,
                    heartbeat = ?DIGEST_HEARTBEAT,
                    id        = ?DIGEST_ID,
                    secret    = ?DIGEST_SECRET },

  % serialize the digest:
  { ok, SerializedData, _ } = gen_server:call( gossiperl_serialization, { serialize, digest, Digest } ),
  EncryptionResult          = gen_server:call( ?ENCRYPTION(LoadedConfiguration), { encrypt, SerializedData } ),
  ?assertMatch( { ok, _ }, EncryptionResult ),
  { ok, Encrypted }         = EncryptionResult,
  DecryptionResult          = gen_server:call( ?ENCRYPTION(LoadedConfiguration), { decrypt, Encrypted } ),
  { ok, _ }                 = DecryptionResult,
  DeserializeResult = gen_server:call( gossiperl_serialization, { deserialize, SerializedData } ),
  ?assertMatch( { ok, _, _ }, DeserializeResult ),
  { ok, PayloadType, Deserialized } = DeserializeResult,
  ?assertEqual( digest, PayloadType ),
  ?assertMatch( { digest, _, _, _, _, _ }, Deserialized ),
  ?assertEqual( ?DIGEST_NAME, Deserialized#digest.name ),
  ?assertEqual( ?DIGEST_PORT, Deserialized#digest.port ),
  ?assertEqual( ?DIGEST_HEARTBEAT, Deserialized#digest.heartbeat ),
  ?assertEqual( ?DIGEST_ID, Deserialized#digest.id ),
  ?assertEqual( ?DIGEST_SECRET, Deserialized#digest.secret ),
  gossiperl_sup:remove_overlay( ?OVERLAY, LoadedConfiguration ).
