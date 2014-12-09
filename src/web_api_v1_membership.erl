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

-module(web_api_v1_membership).

-export([init/2]).

-include("gossiperl.hrl").

init( Req, Opts ) ->
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
          OutMembership = [ [
              { name, Member#digestMember.member_name },
              { state, list_to_binary(atom_to_list(Status)) },
              { ip, Member#digestMember.member_ip },
              { port, Member#digestMember.member_port },
              { heartbeat, Member#digestMember.member_heartbeat }
          ] || { _Reachability, Status, Member } <- gen_server:call( ?MEMBERSHIP( OverlayConfig ), list_members_with_state ) ],
          Response = jsx:encode( [
            { membership, OutMembership },
            { operation, <<"membership">> },
            { overlay, RequestedOverlayName },
            { timestamp, gossiperl_common:get_timestamp() }
          ] ),
          cowboy_req:reply(200, [
            {<<"content-type">>, <<"application/json; charset=utf-8">>}
          ], Response, Req);
        { error, token_mismatch } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req);
        { error, no_auth } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req)
      end;
    { error, no_config } ->
      cowboy_req:reply(404, Req)
  end.
