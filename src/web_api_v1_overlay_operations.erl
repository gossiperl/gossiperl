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

-module(web_api_v1_overlay_operations).

-export([init/2]).

-include("gossiperl.hrl").

init(Req, Opts) ->
  {ok, reply(cowboy_req:method(Req), Req), Opts}.

reply(<<"POST">>, Req) ->
  
  case gen_server:call( gossiperl_web, { authorize, rest_user, Req } ) of
    authorized ->
      {ok, Data, _} = cowboy_req:body(Req, []),
      RequestedOverlayName = cowboy_req:binding(overlay, Req),
      case jsx:decode( Data ) of
        JsonData ->
          case gossiperl_configuration:configuration_from_json( JsonData, list_to_atom( binary_to_list( RequestedOverlayName ) ) ) of
            { error, { Code, Cause } } ->

              cowboy_req:reply(409, [], list_to_binary(io_lib:format("Configuration property problematic ~p, ~p.", [Cause, Code])), Req);

            { ok, ConfigurationRecord } ->

              case gossiperl_sup:add_overlay( list_to_atom( binary_to_list( RequestedOverlayName ) ), ConfigurationRecord ) of
                { ok, _ChildSpec } ->
                  { ok, { _, RecordedConfig } } = gossiperl_configuration:for_overlay( RequestedOverlayName ),
                  gen_server:cast( gossiperl_statistics, { ensure_storage, RequestedOverlayName } ),
                  cowboy_req:reply(201, [
                    {<<"content-type">>, <<"application/json; charset=utf-8">>},
                    {<<"x-session-token">>, RecordedConfig#overlayConfig.internal#internalConfig.webToken}
                  ], jsx:encode( [
                    { token, RecordedConfig#overlayConfig.internal#internalConfig.webToken },
                    { overlays, gossiperl_sup:list_overlays() },
                    { operation, <<"add_overlay">> },
                    { timestamp, gossiperl_common:get_timestamp() }
                  ] ), Req);
                { ok, _ChildSpec, _Info } ->
                  { ok, { _, RecordedConfig } } = gossiperl_configuration:for_overlay( RequestedOverlayName ),
                  gen_server:cast( gossiperl_statistics, { ensure_storage, RequestedOverlayName } ),
                  cowboy_req:reply(201, [
                    {<<"content-type">>, <<"application/json; charset=utf-8">>},
                    {<<"x-session-token">>, RecordedConfig#overlayConfig.internal#internalConfig.webToken}
                  ], jsx:encode( [
                    { token, RecordedConfig#overlayConfig.internal#internalConfig.webToken },
                    { overlay, RequestedOverlayName },
                    { operation, <<"add_overlay">> },
                    { timestamp, gossiperl_common:get_timestamp() }
                  ] ), Req);
                { error, Error } ->
                  case Error of
                    already_present ->
                      cowboy_req:reply(409, [], <<"Overlay with a given name already present.">>, Req);
                    {already_started, _ChildSpec} ->
                      cowboy_req:reply(409, [], <<"Overlay with a given name already started.">>, Req);
                    no_rack_seeds ->
                      cowboy_req:reply(412, [], <<"No seeds for overlay given.">>, Req);
                    AnyOther ->
                      cowboy_req:reply(500, [], list_to_binary( io_lib:format("Error while adding an overlay: ~p", [AnyOther]) ), Req)
                  end
              end

          end

      end;
    { error, not_configured } ->
      cowboy_req:reply(412, [], <<"REST user not configured.">>, Req);
    { error, no_auth } ->
      cowboy_req:reply(401, [
        {<<"www-authenticate">>, <<"Basic realm=\"Add an overlay\"">>}
      ], <<"Authorization required.">>, Req)
  end;

reply(<<"PUT">>, Req) ->
  {ok, Data, _} = cowboy_req:body(Req, []),
  RequestedOverlayName = cowboy_req:binding(overlay, Req),
  case gossiperl_configuration:for_overlay( RequestedOverlayName ) of
    { ok, { _, ExistingConfig } } ->
      case gen_server:call( gossiperl_web, { authorize_token, ExistingConfig, Req } ) of
        { ok, token_ok } ->
          gossiperl_log:info("Incoming reconfiguration data is: ~p", [ Data ]),
          try
            case jsx:decode( Data ) of
              JsonData ->
                MaybePropertyName = cowboy_req:binding(property, Req),
                process_configuration_from_json( Req, JsonData, RequestedOverlayName, ExistingConfig, MaybePropertyName )
            end
          catch
            Error:Reason ->
              error_logger:error_msg("JSON configuration failed: ~p, ~p", [ Error, Reason ]),
              cowboy_req:reply(400, [], <<"Error while parsing JSON data.">>, Req)
          end;
        { error, token_mismatch } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Update configuration for ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req);
        { error, no_auth } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Update configuration for ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req)
      end;
    { error, no_config } ->
      cowboy_req:reply(404, Req)
  end;

reply(<<"DELETE">>, Req) ->

  RequestedOverlayName = cowboy_req:binding(overlay, Req),
  case gossiperl_configuration:for_overlay( RequestedOverlayName ) of
    { ok, { _, ExistingConfig } } ->
      case gen_server:call( gossiperl_web, { authorize_token, ExistingConfig, Req } ) of
        { ok, token_ok } ->
          case gossiperl_sup:remove_overlay( list_to_atom( binary_to_list( RequestedOverlayName ) ), ExistingConfig ) of
            true ->
              Response = jsx:encode( [
                { overlays, gossiperl_sup:list_overlays() },
                { operation, <<"remove_overlay">> },
                { timestamp, gossiperl_common:get_timestamp() }
              ] ),
              cowboy_req:reply(202, [
                {<<"content-type">>, <<"application/json; charset=utf-8">>}
              ], Response, Req);
            { error, Reason } ->
              cowboy_req:reply(409, [], list_to_binary( io_lib:format("Error while removing overlay: ~p", [Reason]) ), Req)
          end;
        { error, token_mismatch } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Delete overlay ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req);
        { error, no_auth } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Delete overlay ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req)
      end;
    { error, no_config } ->
      cowboy_req:reply(404, Req)
  end;

reply(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

process_configuration_from_json(Request, JsonData, RequestedOverlayName, ExistingConfig, MaybePropertyName) when is_atom(MaybePropertyName) andalso MaybePropertyName == undefined ->
  OverlayNameAtom = list_to_atom( binary_to_list( RequestedOverlayName ) ),
  case gossiperl_configuration:configuration_from_json( JsonData, OverlayNameAtom ) of
    { error, { Code, Cause } } ->
      cowboy_req:reply(409, [], list_to_binary(io_lib:format("Configuration property problematic ~p, ~p.", [Cause, Code])), Request);
    {ok, ConfigurationRecord } ->
      NewConfigToMatch = { ConfigurationRecord#overlayConfig.member_name, ConfigurationRecord#overlayConfig.ip, ConfigurationRecord#overlayConfig.iface, ConfigurationRecord#overlayConfig.port },
      OldConfigToMatch = { ExistingConfig#overlayConfig.member_name, ExistingConfig#overlayConfig.ip, ExistingConfig#overlayConfig.iface, ExistingConfig#overlayConfig.port },
      case NewConfigToMatch of
        OldConfigToMatch ->
          case gossiperl_sup:reconfigure_overlay( ExistingConfig, ConfigurationRecord ) of
            { ok, _, _ } ->
              cowboy_req:reply(200, [
                {<<"content-type">>, <<"application/json; charset=utf-8">>}
              ], jsx:encode( [
                { overlay, RequestedOverlayName },
                { operation, <<"reconfigure_overlay">> },
                { timestamp, gossiperl_common:get_timestamp() }
              ] ), Request);
            { error, Reason } ->
              case Reason of
                no_rack_seeds ->
                  cowboy_req:reply(412, [], <<"No seeds for overlay given.">>, Request);
                AnyOther ->
                  cowboy_req:reply(500, [], list_to_binary( io_lib:format("Error while reconfiguring overlay: ~p", [AnyOther]) ), Request)
              end
          end;
        _ ->
          cowboy_req:reply(409, [], <<"One of the properties has changed: ip, iface, port or member_name. Not allowed. To change any of these you must remove and add overlay again.">>, Request)
      end
  end;

process_configuration_from_json(Request, JsonData, RequestedOverlayName, ExistingConfig, MaybePropertyName) when is_binary(MaybePropertyName) ->
  case gossiperl_configuration:configuration_property_from_json( MaybePropertyName, JsonData, ExistingConfig ) of
    { error, { Code, Cause } } ->
      cowboy_req:reply(409, [], list_to_binary(io_lib:format("Configuration property problematic ~p, ~p.", [Cause, Code])), Request);
    ConfigurationRecord ->
      NewConfigToMatch = { ConfigurationRecord#overlayConfig.member_name, ConfigurationRecord#overlayConfig.ip, ConfigurationRecord#overlayConfig.iface, ConfigurationRecord#overlayConfig.port },
      OldConfigToMatch = { ExistingConfig#overlayConfig.member_name, ExistingConfig#overlayConfig.ip, ExistingConfig#overlayConfig.iface, ExistingConfig#overlayConfig.port },
      case NewConfigToMatch of
        OldConfigToMatch ->
          case gossiperl_sup:reconfigure_overlay( ExistingConfig, ConfigurationRecord ) of
            { ok, _, _ } ->
              cowboy_req:reply(202, [
                {<<"content-type">>, <<"application/json; charset=utf-8">>}
              ], jsx:encode( [
                { overlay, RequestedOverlayName },
                { operation, <<"reconfigure_overlay">> },
                { property, MaybePropertyName },
                { timestamp, gossiperl_common:get_timestamp() }
              ] ), Request);
            { error, Reason } ->
              case Reason of
                no_rack_seeds ->
                  cowboy_req:reply(412, [], <<"No seeds for overlay given.">>, Request);
                AnyOther ->
                  cowboy_req:reply(500, [], list_to_binary( io_lib:format("Error while reconfiguring overlay: ~p", [AnyOther]) ), Request)
              end
          end;
        _ ->
          cowboy_req:reply(409, [], <<"One of the properties has changed: ip, iface, port or member_name. Not allowed. To change any of these you must remove and add overlay again.">>, Request)
      end
  end.