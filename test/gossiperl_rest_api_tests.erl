-module(gossiperl_rest_api_tests).
-include_lib("eunit/include/eunit.hrl").

-include_lib("gossiperl_tests.hrl").

-define(GOSSIPERL_HOST, <<"https://127.0.0.1:8080">>).
-define(OVERLAY_NAME, <<"test_overlay">>).
-define(NON_EXISTING_OVERLAY_NAME, <<"non_existing_overlay_name">>).
-define(ETS_NAME, test_storage).

gossiperl_rest_api_test_() ->
  {setup, fun start/0, fun stop/1, [
    fun test_no_credentials/0,
    fun test_list_overlays/0,
    fun test_create_overlay/0,
    fun test_tokens/0,
    fun test_number_of_overlays/0,
    fun test_duplicate_overlay_error/0,
    fun test_nonexisting_overlay_error/0,
    fun test_incorrect_token/0,
    fun test_configuration_get/0,
    fun test_reconfigure_overlay/0,
    fun test_membership_get/0,
    fun test_get_subscriptions_no_error/0,
    fun test_custom_digest_broadcast/0,
    fun test_custom_digest_target/0,
    fun test_delete_overlay/0,
    fun test_number_of_overlays_after_removal/0,
    fun test_reconfigure_gossiperl/0
    ] }.

start() ->
  [ begin
      Result = application:start(App),
      error_logger:info_msg("Starting application ~p: ~p", [ App, Result ] )
    end || App <- ?APPLICATIONS ],
  ets:new( ?ETS_NAME, [set, named_table, public]),
  ets:insert( ?ETS_NAME, { rest_credentials, get_rest_credentials() } ),
  ets:insert( ?ETS_NAME, { superuser_credentials, get_superuser_credentials() } ),
  ok.

stop(_State) ->
  [ application:stop(App) || App <- ?APPLICATIONS ],
  ets:delete(?ETS_NAME),
  noreply.

get_rest_credentials() ->
  {ok, [{ <<"username">>, Username }, { <<"password">>, Password }]} = application:get_env( gossiperl, rest_user ),
  #testCredentials{ username = Username, password = Password }.

get_superuser_credentials() ->
  {ok, [{ <<"username">>, Username }, { <<"password">>, Password }]} = application:get_env( gossiperl, superuser ),
  #testCredentials{ username = Username, password = Password }.

get_auth_header(#testCredentials{ username = Username, password = Password }) ->
  Encoded = base64:encode( <<Username/binary, ":", Password/binary>> ),
  [ { <<"Authorization">>, <<"Basic ", Encoded/binary>> } ].

get_session_header(OverlayToken) when is_binary(OverlayToken) ->
  [ { <<"x-session-token">>, OverlayToken } ].

test_no_credentials() ->
  HackneyRequestResult = hackney:request( post,
                                          <<?GOSSIPERL_HOST/binary, "/overlays/", ?OVERLAY_NAME/binary>>,
                                          [ { <<"content-type">>, <<"application/json">> } ],
                                          ?BIN_CONFIG_VALID,
                                          ?HACKNEY_OPTIONS ),
  ?assertMatch({ ok, _, _, _ }, HackneyRequestResult),
  { ok, StatusCode, Headers, _ } = HackneyRequestResult,
  ?assertEqual( 401, StatusCode ),
  ?assertMatch( { <<"www-authenticate">>, _ }, lists:keyfind(<<"www-authenticate">>, 1, Headers) ).

test_list_overlays() ->
  Credentials = ets:lookup_element( ?ETS_NAME, rest_credentials, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/overlays">>,
                                          get_auth_header(Credentials) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 200, StatusCode ).

test_create_overlay() ->
  Credentials = ets:lookup_element( ?ETS_NAME, rest_credentials, 2 ),
  HackneyRequestResult = hackney:request( post,
                                          <<?GOSSIPERL_HOST/binary, "/overlays/", ?OVERLAY_NAME/binary>>,
                                          get_auth_header(Credentials) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          ?BIN_CONFIG_VALID,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, Headers, _ } = HackneyRequestResult,
  ?assertEqual( 201, StatusCode ),
  ?assertMatch( { <<"x-session-token">>, _ }, lists:keyfind(<<"x-session-token">>, 1, Headers) ),
  { <<"x-session-token">>, OverlaySessionToken } = lists:keyfind(<<"x-session-token">>, 1, Headers),
  ets:insert( ?ETS_NAME, { overlay_token, OverlaySessionToken } ).

test_tokens() ->
  Credentials = ets:lookup_element( ?ETS_NAME, superuser_credentials, 2 ),
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/tokens">>,
                                          get_auth_header(Credentials) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, _, _, BodyRef } = HackneyRequestResult,
  { ok, Body }          = hackney:body(BodyRef),
  ParsedResponse        = jsx:decode( Body ),
  MaybeTokens           = lists:keyfind(<<"tokens">>, 1, ParsedResponse),
  ?assertMatch( { <<"tokens">>, [ [ { ?OVERLAY_NAME, OverlayToken } ] ] }, MaybeTokens ).

test_number_of_overlays() ->
  Credentials = ets:lookup_element( ?ETS_NAME, rest_credentials, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/overlays">>,
                                          get_auth_header(Credentials) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, _, _, BodyRef } = HackneyRequestResult,
  { ok, Body }          = hackney:body(BodyRef),
  ParsedResponse        = jsx:decode( Body ),
  MaybeOverlays = lists:keyfind(<<"overlays">>, 1, ParsedResponse),
  ?assertMatch( { <<"overlays">>, [ ?OVERLAY_NAME ] }, MaybeOverlays ).

test_duplicate_overlay_error() ->
  Credentials = ets:lookup_element( ?ETS_NAME, rest_credentials, 2 ),
  HackneyRequestResult = hackney:request( post,
                                          <<?GOSSIPERL_HOST/binary, "/overlays/", ?OVERLAY_NAME/binary>>,
                                          get_auth_header(Credentials) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          ?BIN_CONFIG_VALID,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 409, StatusCode ).

test_nonexisting_overlay_error() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/configuration/", ?NON_EXISTING_OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 404, StatusCode ).

test_incorrect_token() ->
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/configuration/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(<<"wrong-token">>) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 401, StatusCode ).

test_configuration_get() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/configuration/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, BodyRef } = HackneyRequestResult,
  ?assertEqual( 200, StatusCode ),
  { ok, Body }                   = hackney:body(BodyRef),
  ParsedResponse                 = jsx:decode( Body ),
  MaybeConfiguration             = lists:keyfind(<<"configuration">>, 1, ParsedResponse),
  ?assertMatch( { <<"configuration">>, _ }, MaybeConfiguration ),
  { <<"configuration">>, ConfigData } = MaybeConfiguration,
  MaybeRackName                  = lists:keyfind( <<"rack_name">>, 1, ConfigData ),
  ?assertMatch( { <<"rack_name">>, _ }, MaybeRackName ),
  { <<"rack_name">>, RackName } = MaybeRackName,
  MaybeRacks                     = lists:keyfind( <<"racks">>, 1, ConfigData ),
  ?assertMatch( { <<"racks">>, [ { RackName, [ ?SEED_IP ] } ] }, MaybeRacks ).

test_reconfigure_overlay() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),

  HackneyPostRequestResult = hackney:request( put,
                                              <<?GOSSIPERL_HOST/binary, "/overlays/", ?OVERLAY_NAME/binary, "/racks">>,
                                              get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                              ?RACKS_RECONFIG,
                                              ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyPostRequestResult,
  ?assertEqual( 202, StatusCode ),
  
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/configuration/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode2, _, BodyRef } = HackneyRequestResult,
  ?assertEqual( 200, StatusCode2 ),
  { ok, Body }                    = hackney:body(BodyRef),
  ParsedResponse                  = jsx:decode( Body ),
  MaybeConfiguration              = lists:keyfind(<<"configuration">>, 1, ParsedResponse),
  ?assertMatch( { <<"configuration">>, _ }, MaybeConfiguration ),
  { <<"configuration">>, ConfigData } = MaybeConfiguration,
  MaybeRackName                   = lists:keyfind( <<"rack_name">>, 1, ConfigData ),
  ?assertMatch( { <<"rack_name">>, _ }, MaybeRackName ),
  { <<"rack_name">>, RackName }   = MaybeRackName,
  MaybeRacks                      = lists:keyfind( <<"racks">>, 1, ConfigData ),
  ?assertMatch( { <<"racks">>, [ { RackName, [ ?SEED_IP_RECONFIG ] } ] }, MaybeRacks ),
  timer:sleep(2000).

test_membership_get() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/membership/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, BodyRef } = HackneyRequestResult,
  ?assertEqual( 200, StatusCode ),
  { ok, Body }                   = hackney:body(BodyRef),
  ParsedResponse                 = jsx:decode( Body ),
  MaybeMembership                = lists:keyfind(<<"membership">>, 1, ParsedResponse),
  ?assertMatch( { <<"membership">>, _ }, MaybeMembership ),
  { <<"membership">>, MembershipData } = MaybeMembership,
  ?assertEqual( 1, length( MembershipData ) ).

test_get_subscriptions_no_error() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/subscriptions/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 200, StatusCode ).

test_custom_digest_broadcast() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( post,
                                          <<?GOSSIPERL_HOST/binary, "/digest/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          ?CUSTOM_DIGEST_BRAODCAST,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 202, StatusCode ).

test_custom_digest_target() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( post,
                                          <<?GOSSIPERL_HOST/binary, "/digest/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          ?CUSTOM_DIGEST_TARGET,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 202, StatusCode ).


test_delete_overlay() ->
  OverlayToken = ets:lookup_element( ?ETS_NAME, overlay_token, 2 ),
  HackneyRequestResult = hackney:request( delete,
                                          <<?GOSSIPERL_HOST/binary, "/overlays/", ?OVERLAY_NAME/binary>>,
                                          get_session_header(OverlayToken) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 202, StatusCode ).

test_number_of_overlays_after_removal() ->
  Credentials = ets:lookup_element( ?ETS_NAME, rest_credentials, 2 ),
  HackneyRequestResult = hackney:request( get,
                                          <<?GOSSIPERL_HOST/binary, "/overlays">>,
                                          get_auth_header(Credentials) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, _, _, BodyRef } = HackneyRequestResult,
  { ok, Body }          = hackney:body(BodyRef),
  ParsedResponse        = jsx:decode( Body ),
  MaybeOverlays = lists:keyfind(<<"overlays">>, 1, ParsedResponse),
  ?assertMatch( { <<"overlays">>, [] }, MaybeOverlays ).

test_reconfigure_gossiperl() ->
  Credentials = ets:lookup_element( ?ETS_NAME, superuser_credentials, 2 ),
  HackneyRequestResult = hackney:request( post,
                                          <<?GOSSIPERL_HOST/binary, "/reconfigure">>,
                                          get_auth_header(Credentials) ++ [ { <<"content-type">>, <<"application/json">> } ],
                                          <<>>,
                                          ?HACKNEY_OPTIONS ),
  { ok, StatusCode, _, _ } = HackneyRequestResult,
  ?assertEqual( 202, StatusCode ).
