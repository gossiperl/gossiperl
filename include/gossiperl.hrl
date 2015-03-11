-ifndef(_gossiperl_included).
-define(_gossiperl_included, yeah).

-include("gossiperl_types.hrl").

-record(gossiperNames, {
          overlay :: atom(),
          encryption :: atom(),
          membership :: atom(),
          messaging :: atom(),
          subscriptions :: atom(),
          ets_redeliveries :: atom(),
          ets_subscriptions :: atom() }).

-record(internalConfig, {
          nameList :: list(),
          names=#gossiperNames{},
          socket :: pid(),
          local_socket :: pid(),
          knownIps :: list(),
          webToken :: binary() }).

-record(multicastConfig, {
          ip :: tuple(),
          local_iface_address :: tuple(),
          ttl :: integer(),
          local_port :: integer() }).

-record(overlayConfig, {
          ip :: tuple(),
          ip_hint :: tuple(),
          iface :: binary(),
          port :: integer(),
          seeds :: list(),
          racks = [ { <<"default">>, [] } ] :: list(),
          rack_name = <<"default">> :: binary(),
          quarantine_after = 20 :: integer(),
          about_to_quarantine_ratio = 0.6 :: float(),
          max_quarantined = 10 :: integer(),
          drop_unreachable_after = 180 :: integer(),
          redelivery_retry_max = 0 :: integer(),
          redelivery_retry_every = 5 :: integer(),
          gossip_round_every = 2 :: integer(),
          drop_stale_subscriptions_after = 20 :: integer(),
          incoming_data_buffer_size = 16777216 :: integer(),
          outgoing_data_buffer_size = 16777216 :: integer(),
          read_packet_count = 5 :: integer(),
          member_name :: binary(),
          name :: atom(),
          symmetric_key :: binary(),
          secret = <<"default-overlay-secret">> :: binary(),
          internal = #internalConfig{} :: #internalConfig{},
          multicast = undefined :: #multicastConfig{} }).

-record(statsEntryDigestType, {
          key :: tuple(),
          message_in=0 :: integer(),
          message_out=0 :: integer(),
          data_in=0 :: integer(),
          data_out=0 :: integer() }).

-record(statsEntry, {
          timestamp :: tuple(),
          message_in=0 :: integer(),
          message_out=0 :: integer(),
          message_failed_in=0 :: integer(),
          data_in=0 :: integer(),
          data_out=0 :: integer() }).

-record(digestRedelivery, {
          message_id :: binary(),
          client_name :: binary(),
          client_ip :: binary(),
          client_port :: integer(),
          last_delivery_attempt :: integer(),
          retry_count = 0 :: integer(),
          digest :: any() }).

-type gossiperl_config() :: #overlayConfig{}.
-type rack_name() :: binary().
-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
-type timestamp() :: {MegaSecs :: non_neg_integer(), Secs :: non_neg_integer(), MicroSecs :: non_neg_integer()}.
-type delivery_reason() :: delivered | unsubscribed | max_redelivery_attempts.
-type token_auth_error() :: no_auth | token_mismatch.
-type superuser_auth_error() :: no_auth | not_configured.
-type overlay_removal_error() :: running | restarting | not_found | simple_one_for_one.
-type overlay_termination_error() :: not_found | simple_one_for_one.
-type handle_reachable_success_type() :: added | returned | updated.
-type handle_reachable_error_type() :: no_member | no_secret_match | delivery_expired.
-type digest_ack_response() :: no_subscription_delivery | subscriptions_delivered.
-type multicast_setting() :: ip | ttl | local_iface_address.

-define(ETC_CONFIG_PATH, <<"/etc/gossiperl/settings.json">>).
-define(PRIV_CONFIG_PATH, <<"settings.json">>).
-define(LOCAL_ADDRESS_IP, <<"127.0.0.1">>).
-define(REACHABLE, <<"reachable">>).
-define(UNREACHABLE, <<"unreachable">>).
-define(QUARANTINED, <<"quarantined">>).

-define(MEMBER_IN, <<"member_in">>).
-define(MEMBER_QUARANTINE, <<"member_quarantine">>).
-define(MEMBER_UNREACHABLE, <<"member_unreachable">>).
-define(MEMBER_DROP, <<"member_drop">>).
-define(MEMBER_OUT, <<"member_out">>).
-define(MEMBER_REJOIN, <<"member_rejoin">>).
-define(LOCAL_NOTIFICATIONS, [ ?MEMBER_IN, ?MEMBER_QUARANTINE, ?MEMBER_UNREACHABLE, ?MEMBER_DROP, ?MEMBER_OUT, ?MEMBER_REJOIN ]).

-define(MEMBER_CHECK_STATE_EVERY, 1000).

-define(GOSSIPER_MEMBER( ModuleName ), { ModuleName, {ModuleName, start_link, []}, permanent, brutal_kill, worker, [] }).
-define(GOSSIPER_OVERLAY( OverlayName, Config ), { OverlayName, { gossiperl_overlay_sup, start_link, [ Config ]}, permanent, brutal_kill, supervisor, [] }).
-define(OVERLAY_MEMBER( AtomName, ModuleName, Config ), { AtomName, {ModuleName, start_link, [Config]}, permanent, brutal_kill, worker, [] }).

-define(ENCRYPTION(Config), Config#overlayConfig.internal#internalConfig.names#gossiperNames.encryption).
-define(MEMBERSHIP(Config), Config#overlayConfig.internal#internalConfig.names#gossiperNames.membership).
-define(MESSAGING(Config), Config#overlayConfig.internal#internalConfig.names#gossiperNames.messaging).
-define(SUBSCRIPTIONS(Config), Config#overlayConfig.internal#internalConfig.names#gossiperNames.subscriptions).

-define(ETS_REDELIVERIES(Config), Config#overlayConfig.internal#internalConfig.names#gossiperNames.ets_redeliveries).
-define(ETS_SUBSCRIPTIONS(Config), Config#overlayConfig.internal#internalConfig.names#gossiperNames.ets_subscriptions).
-define(MULTICAST_OPTS(Config), case Config#overlayConfig.multicast of
                                  undefined -> [];
                                  _         -> [ {multicast_ttl, Config#overlayConfig.multicast#multicastConfig.ttl},
                                                 {multicast_loop, true},
                                                 {broadcast, true},
                                                 {add_membership, {Config#overlayConfig.multicast#multicastConfig.ip, Config#overlayConfig.multicast#multicastConfig.local_iface_address}} ]
                                end ).
-define(INET_OPTS(Config), [ {recbuf, Config#overlayConfig.incoming_data_buffer_size},
                             {sndbuf, Config#overlayConfig.outgoing_data_buffer_size},
                             {read_packets, Config#overlayConfig.read_packet_count} ] ).
-define(AES_PAD(Bin), <<Bin/binary, 0:(( 32 - ( byte_size(Bin) rem 32 ) ) *8 )>>).

-define(DEFAULT_MULTICAST_TTL, 4).
-define(DEFAULT_MULTICAST_LOCAL_IF_ADDR, {0,0,0,0}).

-endif.