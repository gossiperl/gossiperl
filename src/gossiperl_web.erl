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

-module(gossiperl_web).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("gossiperl.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  
  NumAcceptors = case application:get_env( gossiperl, rest_num_acceptors ) of
    { ok, Value1 } -> Value1;
    _              -> 100
  end,

  Port = case application:get_env( gossiperl, rest_port ) of
    { ok, Value2 } -> Value2;
    _              -> 8080
  end,

  CowboyArguments = case application:get_env( gossiperl, rest_ssl ) of
    { ok, [ { <<"certfile">>, CertFile }, { <<"keyfile">>, KeyFile } ] } ->
      [ {port, Port }, { certfile, CertFile }, { keyfile, KeyFile } ];
    _ ->
      [ {port, Port } ]
  end,

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/overlays", web_api_v1_overlays, []}, %% GET
      {"/membership/:overlay", [], web_api_v1_membership, []}, %% GET
      {"/configuration/:overlay", [], web_api_v1_configuration, []}, %% GET
      {"/statistics/:overlay/[:window]", [], web_api_v1_statistics, []}, %% GET
      {"/subscriptions/:overlay", [], web_api_v1_subscriptions, []}, %% GET
      {"/overlays/:overlay/[:property]", [], web_api_v1_overlay_operations, []}, %% POST, PUT, DELETE
      {"/digest/:overlay", [], web_api_v1_digest, []}, %% POST
      {"/tokens", [], web_api_v1_tokens, []}, %% GET
      {"/reconfigure", [], web_api_v1_reconfigure, []} %% POST
    ]}
  ]),

  case lists:keyfind( certfile, 1, CowboyArguments ) of
    { certfile, _ } ->
      {ok, _} = cowboy:start_https( https,
                                    NumAcceptors,
                                    CowboyArguments,
                                    [ {compress, true}, {env, [{dispatch, Dispatch}]} ] ),
      gossiperl_log:notice("[REST]: Gossiperl REST running in HTTPS, port ~p", [ Port ]);
    false ->
      {ok, _} = cowboy:start_http(  http,
                                    NumAcceptors,
                                    CowboyArguments,
                                    [ {compress, true}, {env, [{dispatch, Dispatch}]} ] ),
      gossiperl_log:notice("[REST]: Gossiperl REST running in HTTP, port ~p", [ Port ])
  end,
  
  {ok, Dispatch}.

stop() -> gen_server:cast(?MODULE, stop).

terminate(_Reason, _LoopData) ->
  {ok}.

handle_cast(stop, LoopData) ->
  {noreply, LoopData}.

handle_info(_Info, LoopData) ->
  {noreply, LoopData}.

handle_call({ authorize, CredentialSet, Req }, From, LoopData) ->
  case application:get_env( gossiperl, CredentialSet ) of
    { ok, [ { <<"username">>, RestUser }, { <<"password">>, RestPass } ] } ->
      case credentials(Req) of
        { RestUser, RestPass, Req } -> gen_server:reply( From, authorized );
        _                           -> gen_server:reply( From, { error, no_auth } )
      end;
    _ -> gen_server:reply( From, { error, not_configured } )
  end,
  { noreply, LoopData };

handle_call({ authorize_token, Config, Req }, From, LoopData) ->
  gen_server:reply( From, token_authentication(Config, Req) ),
  { noreply, LoopData };

handle_call(_Msg, _From, LoopData) ->
  {reply, ok, LoopData}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc Checks the existence of Authorization header.
-spec credentials( Req :: cowboy_req:req() ) -> { binary(), binary(), cowboy_req:req() } | { undefined, undefined, cowboy_req:req() }.
credentials(Req) ->
  AuthorizationHeader = cowboy_req:header( <<"authorization">>, Req, undefined ),
  case AuthorizationHeader of
    undefined ->
      {undefined, undefined, Req};
    _ ->
      {Username, Password} = credentials_from_header(AuthorizationHeader),
      {Username, Password, Req}
  end.

%% @doc Reads encoded Basic credentials from the Authorization header.
-spec credentials_from_header( binary() ) -> { binary(), binary() } | { undefined, undefined }.
credentials_from_header(AuthorizationHeader) when is_binary( AuthorizationHeader ) ->
  case binary:split(AuthorizationHeader, <<" ">>) of
    [<<"Basic">>, EncodedCredentials] -> decoded_credentials(EncodedCredentials);
    _                                 -> {undefined, undefined}
  end.

%% @doc Reads encoded Basic credentials from the Authorization header.
-spec decoded_credentials( binary() ) -> { binary(), binary() } | { undefined, undefined }.
decoded_credentials(EncodedCredentials) when is_binary( EncodedCredentials ) ->
  DecodedCredentials = base64:decode(EncodedCredentials),
  case binary:split(DecodedCredentials, <<$:>>) of
    [Username, Password] -> {Username, Password};
    _                    -> {undefined, undefined}
  end.

%% @doc Attempts token authentication, first tests the X-Session-Token header, then Authorization.
-spec token_authentication( gossiperl_config(), cowboy_req:req() ) -> { ok, token_ok } | { error, token_auth_error() }.
token_authentication(Config, Req) ->
  %stored token found for the overlay, check if x-session-token header given:
  XSessionToken = cowboy_req:header( <<"x-session-token">>, Req, undefined ),
  case XSessionToken of
    undefined ->
      % there is no x-session-token, check if basic auth?:
      AuthorizationHeader = cowboy_req:header( <<"authorization">>, Req, undefined ),
      case AuthorizationHeader of
        undefined -> { error, no_auth };
        _         -> AuthAgainst = { list_to_binary( Config#overlayConfig.internal#internalConfig.nameList ),
                                     Config#overlayConfig.internal#internalConfig.webToken },
                     case credentials_from_header(AuthorizationHeader) of
                       AuthAgainst -> { ok, token_ok };
                       _           -> { error, token_mismatch }
                     end
      end;
    _ ->
      % yes, x-session-token header found, check if token correct:
      case Config#overlayConfig.internal#internalConfig.webToken of
        XSessionToken -> { ok, token_ok };
        _             -> { error, token_mismatch }
      end
  end.
