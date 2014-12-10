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

-module(web_api_v1_statistics).

-export([init/2]).

-include("gossiperl.hrl").

init(Req, Opts) ->
  {ok, reply(cowboy_req:method(Req), Req), Opts}.

reply(<<"GET">>, Req) ->
  RequestedOverlayName = cowboy_req:binding(overlay, Req),
  case gossiperl_configuration:for_overlay( RequestedOverlayName ) of
    { ok, { _, OverlayConfig } } ->
      case gen_server:call( gossiperl_web, { authorize_token, OverlayConfig, Req } ) of
        { ok, token_ok } ->
          RequestedWindow = cowboy_req:binding(window, Req, <<"total">>),
          AllowedWindows = [ <<"total">>, <<"1d">>, <<"12h">>, <<"6h">>, <<"3h">>, <<"1h">>, <<"30m">>, <<"10m">>, <<"1m">> ],
          case lists:member( RequestedWindow, AllowedWindows ) of
            true ->
              StatisticsResult = gen_server:call( gossiperl_statistics, { get_summary, RequestedOverlayName, RequestedWindow } ),
              case StatisticsResult of
                not_running ->
                  cowboy_req:reply(200, [
                    {<<"content-type">>, <<"application/json; charset=utf-8">>}
                  ], jsx:encode([
                    {overlay, RequestedOverlayName},
                    {window, RequestedWindow},
                    {enabled, false},
                    {statistics, nil},
                    {ts, gossiperl_common:get_timestamp()}
                  ]), Req);
                _ ->
                  cowboy_req:reply(200, [
                    {<<"content-type">>, <<"application/json; charset=utf-8">>}
                  ], jsx:encode([
                    {overlay, RequestedOverlayName},
                    {window, RequestedWindow},
                    {enabled, true},
                    {statistics, jsx:encode( StatisticsResult )},
                    {ts, gossiperl_common:get_timestamp()}
                  ]), Req)
              end;
            false ->
              cowboy_req:reply(412, [], <<"Requested window ~p is not supported.">>, Req)
          end;
        { error, token_mismatch } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Statistics of ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req);
        { error, no_auth } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Statistics of ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req)
      end;
    { error, no_config } ->
      cowboy_req:reply(404, Req)
  end;
  
reply(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).
