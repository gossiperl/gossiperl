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

-module(web_api_v1_configuration).

-export([init/2]).

-include("gossiperl.hrl").

init(Req, Opts) ->
  {ok, reply(cowboy_req:method(Req), Req), Opts}.

reply(<<"GET">>, Req) ->
  reply_with_data(data, Req);
reply(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

reply_with_data(data, Req) ->
  RequestedOverlayName = cowboy_req:binding(overlay, Req),

  case gossiperl_configuration:for_overlay( RequestedOverlayName ) of
    { ok, { _, OverlayConfig } } ->

      case gen_server:call( gossiperl_web, { authorize_token, OverlayConfig, Req } ) of
        { ok, token_ok } ->

          OutConfiguration = [
            { member_name, OverlayConfig#overlayConfig.member_name },
            { ip, list_to_binary( inet:ntoa(OverlayConfig#overlayConfig.ip) ) },
            { port, OverlayConfig#overlayConfig.port },
            { ip_hint, case OverlayConfig#overlayConfig.ip_hint of
                            {_,_,_,_} ->
                              list_to_binary( inet:ntoa( OverlayConfig#overlayConfig.ip_hint ) );
                            _ ->
                              <<"undefined">>
                       end },
            { iface, case OverlayConfig#overlayConfig.iface of
                            undefined ->
                              <<"undefined">>;
                            _ ->
                              OverlayConfig#overlayConfig.iface
                     end },
            { rack_name, OverlayConfig#overlayConfig.rack_name },
            { racks, lists:foldl( fun( { RackName, SeedIps }, Acc ) ->
                                    Acc ++ [ {  RackName, 
                                                lists:foldl(  fun( SeedIp, Acc2 ) ->
                                                                Acc2 ++ [ list_to_binary( inet:ntoa(SeedIp) ) ]
                                                              end, [], SeedIps ) } ]
                                  end, [], OverlayConfig#overlayConfig.racks) },
            { quarantine_after, OverlayConfig#overlayConfig.quarantine_after },
            { max_quarantined, OverlayConfig#overlayConfig.max_quarantined },
            { drop_unreachable_after, OverlayConfig#overlayConfig.drop_unreachable_after },
            { redelivery_retry_max, OverlayConfig#overlayConfig.redelivery_retry_max },
            { redelivery_retry_every, OverlayConfig#overlayConfig.redelivery_retry_every },
            { gossip_round_every, OverlayConfig#overlayConfig.gossip_round_every },
            { drop_stale_subscriptions_after, OverlayConfig#overlayConfig.drop_stale_subscriptions_after },
            { incoming_data_buffer_size, OverlayConfig#overlayConfig.incoming_data_buffer_size },
            { outgoing_data_buffer_size, OverlayConfig#overlayConfig.outgoing_data_buffer_size },
            { read_packet_count, OverlayConfig#overlayConfig.read_packet_count },
            { secret, <<"protected">> },
            { symmetric_key, <<"protected">> },
            { iv, <<"protected">> }
          ],
          Response = jsx:encode( [
            { configuration, OutConfiguration },
            { operation, <<"configuration">> },
            { overlay, RequestedOverlayName },
            { timestamp, gossiperl_common:get_timestamp() }
          ] ),
          cowboy_req:reply(200, [
            {<<"content-type">>, <<"application/json; charset=utf-8">>}
          ], Response, Req);
        { error, token_mismatch } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Configuration of ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req);
        { error, no_auth } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Configuration of ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req)
      end;
    { error, no_config } ->
      cowboy_req:reply(404, Req)
  end.
