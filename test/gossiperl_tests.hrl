-ifndef(_gossiperl_tests_stuff_included).
-define(_gossiperl_tests_stuff_included, yeah).

-record(testCredentials, { username :: binary(),
                           password :: binary() }).

-define(APPLICATIONS, [ asn1, crypto, public_key, erlsha2, jsx, thrift,
                        quickrand, uuid, cowlib, ranch, cowboy,
                        syntax_tools, compiler, goldrush,
                        lager, uuid, ssl, idna, hackney, gossiperl ]).

-define(SEED_IP, <<"127.0.0.1">>).
-define(SEED_IPV6, <<"2001:0db8:0:f101::1">>).

-define(SEED_IP_RECONFIG, <<"192.168.0.1">>).
-define(RACKS_RECONFIG, <<"{ \"dev_rack1\": [\"", ?SEED_IP_RECONFIG/binary ,"\"] }">>).

-define(BIN_CONFIG_VALID, <<"{ \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"", ?SEED_IP/binary ,"\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_VALID_IPV6, <<"{ \"ip\": \"::\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"", ?SEED_IPV6/binary ,"\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_VALID_MULTICAST, <<"{ \"port\": 6666, \"multicast\": { \"ip\": \"224.0.0.251\", \"ttl\": 4, \"local_iface_address\": \"0.0.0.0\" }, \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_VALID_MULTICAST_NO_RACKS, <<"{ \"port\": 6666, \"multicast\": { \"ip\": \"224.0.0.251\", \"ttl\": 4, \"local_iface_address\": \"0.0.0.0\" }, \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_MEMBER_NAME, <<"{ \"member_name\": \"hello_world\", \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"127.0.0.1\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_MISSING_SEEDS, <<"{ \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"some_other_rack\": [\"127.0.0.1\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_INVALID1, <<"{ \"ip\": \"0.0.0.0\", \"port\": \"6666\", \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"127.0.0.1\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_INVALID2, <<"{ \"ip\": \"no an ip address\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"127.0.0.1\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_INVALID3, <<"{ \"ip\": \"0.0.0.0\", \"port\": 6666, \"rack_name\": \"dev_rack1\", \"racks\": { \"dev_rack1\": [\"not an ip\"] } , \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).
-define(BIN_CONFIG_INVALID_MULTICAST1, <<"{ \"port\": 6666, \"multicast\": { \"ttl\": 4, \"local_iface_address\": \"0.0.0.0\" }, \"symmetric_key\": \"v3JElaRswYgxOt4b\" }">>).

-define(CUSTOM_DIGEST_TARGET, <<"{\"digestForwardableTest\": [{ \"some_data\": \"some data to send\", \"string\": 1 }], \"recipient_ip\": \"192.168.50.100\", \"recipient_port\": 6666 }">>).
-define(CUSTOM_DIGEST_BRAODCAST, <<"{\"digestForwardableTest\": [{ \"some_data\": \"some data to send\", \"string\": 1 }] }">>).

-define(HACKNEY_OPTIONS, [ insecure ]).

-endif.