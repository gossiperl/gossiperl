%% Copyright (c) 2014 Radoslaw Gruchalski <radek@gruchalski.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(gossiperl_configuration).

-export([
  setup/1,
  for_overlay/1,
  overlay_socket/2,
  overlay_socket/3,
  remove_configuration_for/1,
  list_overlays/0,
  configuration_from_json/2,
  configuration_property_from_json/3,
  store_config/1,
  validate_racks/1]).

-include("gossiperl.hrl").

%% @doc Removes configuration for an overlay.
-spec remove_configuration_for( atom() ) -> true.
remove_configuration_for(OverlayName) when is_atom(OverlayName) ->
  ets:delete(gossiperl_overlay_configuration, list_to_binary(atom_to_list( OverlayName )) ).

%% @doc Begins configuring an overlay.
-spec setup( gossiperl_config() ) -> gossiperl_config().
setup(Config = #overlayConfig{}) ->
  Config2 = case Config#overlayConfig.member_name of
    undefined ->
      Config#overlayConfig{ member_name=list_to_binary(
        atom_to_list(Config#overlayConfig.name) ++
        inet:ntoa(Config#overlayConfig.ip) ++
        ":" ++ integer_to_list(Config#overlayConfig.port) ++
        "@" ++ uuid:uuid_to_string(uuid:get_v4())
      ) };
    _ ->
      Config
  end,
  store_config(
    names(
      network_adapter(
        Config2#overlayConfig{
          internal = Config2#overlayConfig.internal#internalConfig{
            nameList = atom_to_list(Config2#overlayConfig.name),
            knownIps = gossiperl_common:get_all_ip_addrs(),
            webToken = list_to_binary( uuid:uuid_to_string(uuid:get_v4()) )
          }
        }
      )
    )
  ).

%% @doc Sets up internal overlay process names for given configuration.
-spec names( gossiperl_config() ) -> gossiperl_config().
names(Config = #overlayConfig{}) ->
  Config#overlayConfig{
    internal = Config#overlayConfig.internal#internalConfig{
      names = #gossiperNames{
                    overlay               = list_to_atom( "overlay_" ++ Config#overlayConfig.internal#internalConfig.nameList ),
                    encryption            = list_to_atom( "encryption_" ++ Config#overlayConfig.internal#internalConfig.nameList ),
                    membership            = list_to_atom( "membership_" ++ Config#overlayConfig.internal#internalConfig.nameList ),
                    messaging             = list_to_atom( "messaging_" ++ Config#overlayConfig.internal#internalConfig.nameList ),
                    subscriptions         = list_to_atom( "subscriptions_" ++ Config#overlayConfig.internal#internalConfig.nameList ),
                    ets_redeliveries      = list_to_atom( "ets_redeliveries_" ++ Config#overlayConfig.internal#internalConfig.nameList ),
                    ets_subscriptions     = list_to_atom( "ets_subscriptions_" ++ Config#overlayConfig.internal#internalConfig.nameList ) }
    }
  }.

%% @doc Stores UDP socket of an overlay.
-spec overlay_socket( port(), gossiperl_config() ) -> gossiperl_config().
overlay_socket(Socket, Config) when is_port(Socket) ->
  Config2 = Config#overlayConfig{
    internal = Config#overlayConfig.internal#internalConfig{
      socket = Socket } },
  store_config( Config2 ).

%% @doc Stores UDP socket of an overlay and local communication.
-spec overlay_socket( port(), port(), gossiperl_config() ) -> gossiperl_config().
overlay_socket(Socket, LocalSocket, Config) when is_port(Socket) andalso is_port(LocalSocket) andalso Socket =/= LocalSocket ->
  Config2 = Config#overlayConfig{
    internal = Config#overlayConfig.internal#internalConfig{
      socket       = Socket,
      local_socket = LocalSocket } },
  store_config( Config2 ).

%% @doc Loads the seeds of rack for a configuration.
-spec get_rack_seeds( gossiperl_config() ) -> [ binary() ].
get_rack_seeds( Config = #overlayConfig{} ) ->
  { _, Seeds } = lists:keyfind( Config#overlayConfig.rack_name, 1, Config#overlayConfig.racks ),
  Seeds.

%% @doc If configurig with iface, ensure binding to an IP address of that iface.
-spec network_adapter( gossiperl_config() ) -> gossiperl_config() | { error, term() }.
network_adapter(Config = #overlayConfig{}) ->
  case Config#overlayConfig.iface of
    undefined ->
      Config;
    _ ->
      case gossiperl_common:get_iface_ip( Config#overlayConfig.iface ) of
        { ok, Ip }        -> Config#overlayConfig{ ip=Ip };
        { error, Reason } -> { error, { Reason, Config#overlayConfig.iface } }
      end
  end.

%% @doc Stores configuration of an overlay.
-spec store_config( gossiperl_config() ) -> gossiperl_config().
store_config(Config = #overlayConfig{}) ->
  Config2 = Config#overlayConfig{ seeds = get_rack_seeds( Config ) },
  true = ets:insert(gossiperl_overlay_configuration, { list_to_binary( Config2#overlayConfig.internal#internalConfig.nameList ), Config2 } ),
  Config2.

%% @doc Get configuration for an overlay.
-spec for_overlay( atom() | list() | binary() ) -> { ok, gossiperl_config() } | { error, no_config }.
for_overlay(OverlayName) when is_atom(OverlayName) ->
  for_overlay( atom_to_list( OverlayName ) );
for_overlay(OverlayName) when is_list(OverlayName) ->
  for_overlay( list_to_binary( OverlayName ) );
for_overlay(OverlayName) when is_binary(OverlayName) ->
  case ets:lookup(gossiperl_overlay_configuration, OverlayName) of
    [ Config ] -> { ok, Config };
    []         -> { error, no_config }
  end.

%% @doc List available overlay configurations.
-spec list_overlays() -> [ { atom(), gossiperl_config() } ].
list_overlays() ->
  lists:flatten( ets:match(gossiperl_overlay_configuration, '$1') ).

%% @doc Given the data read and parsed from JSON file, return populated configuration record.
-spec configuration_from_json( [ { binary(), any() } ], atom() ) -> { ok, gossiperl_config() } | { error, { atom(), any() } }.
configuration_from_json( JsonData, OverlayName ) when is_atom(OverlayName) ->
  Configuration = lists:foldl(fun( JsonProperty, Record ) ->
    { FieldName, FieldValue } = JsonProperty,
    case Record of
      {error, {_Reason, _}} ->
        Record;
      _ ->
        configuration_property_from_json( FieldName, FieldValue, Record )
    end
  end, #overlayConfig{}, JsonData),

  case Configuration of
    { error, Any } ->
      { error, Any };
    _ ->
      { ok, Configuration#overlayConfig{ name = OverlayName,
                                         % if multicast, use multicast ip as overlay ip,
                                         % this covers the REST configuration nuisances
                                         ip   = case Configuration#overlayConfig.multicast of
                                                  undefined ->
                                                    Configuration#overlayConfig.ip;
                                                  _ ->
                                                    Configuration#overlayConfig.multicast#multicastConfig.ip
                                                end } }
  end.

-spec configuration_property_from_json( binary(), any(), gossiperl_config() ) -> gossiperl_config() |  { error, { atom(), any() } }.
configuration_property_from_json(<<"member_name">>, FieldValue, ConfigurationRecord) ->
  case as_binary(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ member_name = FieldValue }
  end;

configuration_property_from_json(<<"ip">>, FieldValue, ConfigurationRecord) ->
  case as_ip(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    IpAddress                    -> ConfigurationRecord#overlayConfig{ ip = IpAddress }
  end;

configuration_property_from_json(<<"port">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ port = FieldValue }
  end;

configuration_property_from_json(<<"iface">>, FieldValue, ConfigurationRecord) ->
  case as_binary(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ iface = FieldValue }
  end;

configuration_property_from_json(<<"secret">>, FieldValue, ConfigurationRecord) ->
  case as_binary(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ secret = FieldValue }
  end;

configuration_property_from_json(<<"rack_name">>, FieldValue, ConfigurationRecord) ->
  case as_binary(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ rack_name = FieldValue }
  end;

configuration_property_from_json(<<"ip_hint">>, FieldValue, ConfigurationRecord) ->
  case as_ip(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ ip_hint = FieldValue }
  end;

configuration_property_from_json(<<"racks">>, FieldValue, ConfigurationRecord) ->
  Racks = [ maybe_rack_data( MaybeRackData ) || MaybeRackData <- FieldValue ],
  case lists:keyfind(error, 1, Racks) of
    { error, Reason } -> { error, Reason };
    false             -> ConfigurationRecord#overlayConfig{ racks = Racks }
  end;

configuration_property_from_json(<<"multicast">>, FieldValue, ConfigurationRecord) ->
  MulticastSettings = [ maybe_multicast_setting( { MulticastSettingName, MulticastSettingValue } ) || { MulticastSettingName, MulticastSettingValue } <- FieldValue ],
  case lists:keyfind(error, 1, MulticastSettings) of
    { error, Reason } -> { error, Reason };
    false             -> case proplists:get_value( ip, MulticastSettings, not_found ) of
                           not_found -> { error, { missing_value, multicast_ip } };
                           _         -> ConfigurationRecord#overlayConfig{ multicast = to_multicast_settings( MulticastSettings ) }
                         end
  end;

configuration_property_from_json(<<"quarantine_after">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ quarantine_after = FieldValue }
  end;

configuration_property_from_json(<<"max_quarantined">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ max_quarantined = FieldValue }
  end;

configuration_property_from_json(<<"drop_unreachable_after">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ drop_unreachable_after = FieldValue }
  end;

configuration_property_from_json(<<"redelivery_retry_max">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ redelivery_retry_max = FieldValue }
  end;

configuration_property_from_json(<<"redelivery_retry_every">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ redelivery_retry_every = FieldValue }
  end;

configuration_property_from_json(<<"gossip_round_every">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ gossip_round_every = FieldValue }
  end;

configuration_property_from_json(<<"drop_stale_subscriptions_after">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ drop_stale_subscriptions_after = FieldValue }
  end;

configuration_property_from_json(<<"incoming_data_buffer_size">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ incoming_data_buffer_size = FieldValue }
  end;

configuration_property_from_json(<<"outgoing_data_buffer_size">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ outgoing_data_buffer_size = FieldValue }
  end;

configuration_property_from_json(<<"read_packet_count">>, FieldValue, ConfigurationRecord) ->
  case as_integer(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ read_packet_count = FieldValue }
  end;

configuration_property_from_json(<<"symmetric_key">>, FieldValue, ConfigurationRecord) ->
  case as_binary(FieldValue) of
    { error, { Reason, Cause } } -> { error, { Reason, Cause } };
    _                            -> ConfigurationRecord#overlayConfig{ symmetric_key = FieldValue }
  end;

configuration_property_from_json(FieldName, _, _) ->
  { error, { badarg, FieldName } }.

%% @doc Run additional validation checks of the configuration.
-spec validate_racks( gossiperl_config() ) -> { ok, gossiperl_config() } | { error, no_rack_seeds }.
validate_racks(Configuration = #overlayConfig{ multicast = undefined }) ->
  %% Check that `racks` contains `seeds` for for `rack_name`:
  RackName = Configuration#overlayConfig.rack_name,
  case lists:keyfind( RackName, 1, Configuration#overlayConfig.racks ) of
    { RackName, _ } -> { ok, Configuration };
    false           -> { error, no_rack_seeds }
  end;

validate_racks(Configuration = #overlayConfig{ multicast = _ }) ->
  { ok, Configuration }.

%% @doc Load list op inet IPs from the rack settings.
-spec maybe_rack_data({ rack_name(), [ binary() ] }) -> { rack_name(), [ inet:ip_address() ] } | { error, { atom(), any() } }.
maybe_rack_data({ RackName, SeedList }) when is_binary(RackName) andalso is_list(SeedList) ->
  MaybeSeeds = [ gossiperl_common:parse_binary_ip( BinIp ) || BinIp <- SeedList ],
  case lists:keyfind(error, 1, MaybeSeeds) of
    { error, Reason } -> { error, Reason };
    false             -> { RackName, MaybeSeeds }
  end;

maybe_rack_data(Value) ->
  { error, { rack_settings_invalid, Value } }.

%% @doc Get data as binary or error.
-spec as_binary( any() ) -> binary() | { error, { not_binary, any() } }.
as_binary(Value) ->
  case is_binary(Value) of
    true ->  Value;
    false -> { error, { not_binary, Value } }
  end.

%% @doc Get data as inet IP address.
-spec as_ip( any() ) -> { ok, inet:ip_address() } | { error, { not_binary, any() } } | { error, { not_ip, any() } }.
as_ip(Value) ->
  case as_binary(Value) of
    {error, {R,C_}} -> {error, {R,C_}};
    _               -> case gossiperl_common:parse_binary_ip(Value) of
                         { error, Reason } -> { error, Reason };
                         IpAddress         -> IpAddress
                       end
  end.

%% @doc Get data as integer or error.
-spec as_integer( any() ) -> integer() | { error, { not_integer, any() } }.
as_integer(Value) ->
  case is_integer(Value) of
    true  -> Value;
    false -> { error, { not_integer, Value } }
  end.

%%% Multicast settings:

%% @doc Get single multicast setting.
-spec maybe_multicast_setting( { binary(), binary() | integer() } ) -> { atom(), any() } | { error, { atom(), any() } }.
maybe_multicast_setting( { <<"ip">>, Address } ) ->
  case as_ip(Address) of
    { error, Reason } -> { error, Reason };
    IpAddress         -> { ip, IpAddress }
  end;

maybe_multicast_setting( { <<"local_iface_address">>, Address } ) ->
  case as_ip(Address) of
    { error, Reason } -> { error, Reason };
    IpAddress         -> { local_iface_address, IpAddress }
  end;

maybe_multicast_setting( { <<"ttl">>, Value } ) ->
  case as_integer(Value) of
    { error, Reason } -> { error, Reason };
    TtlValue          -> { ttl, TtlValue }
  end;

maybe_multicast_setting( { <<"local_port">>, Value } ) ->
  case as_integer(Value) of
    { error, Reason } -> { error, Reason };
    LocalPortValue    -> { local_port, LocalPortValue }
  end;

maybe_multicast_setting( AnyOther ) ->
  { error, { unsupported_multicast_setting, AnyOther } }.

%% @doc Convert proplist of multicast settings into a record.
-spec to_multicast_settings([ { multicast_setting(), any() } ]) -> #multicastConfig{} | { error, { atom(), any() } }.
to_multicast_settings(MulticastSettings) ->
  Config = #multicastConfig{
    ip  = proplists:get_value( ip, MulticastSettings, undefined ),
    ttl = proplists:get_value( ttl, MulticastSettings, ?DEFAULT_MULTICAST_TTL ),
    local_iface_address = proplists:get_value( local_iface_address, MulticastSettings, ?DEFAULT_MULTICAST_LOCAL_IF_ADDR ),
    local_port = proplists:get_value( local_port, MulticastSettings, 0 ) },
  case Config#multicastConfig.ip of
    undefined -> { error, { multicast_address, required } };
    _         -> Config
  end.

