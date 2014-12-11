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

-module(gossiperl_subscriptions_qos1_redelivery).

-export([start_link/5, init/5]).

-include("gossiperl.hrl").

%% @doc Starts new QoS1 delivery process.
-spec start_link( gossiperl_config(), #digestMember{}, binary(), #digestEnvelope{}, binary() ) -> pid().
start_link(Config = #overlayConfig{}, Member = #digestMember{}, EventType, EventObject, ForwardedDigestId)
  when is_binary(EventType) andalso is_binary(EventObject)
                            andalso is_binary(ForwardedDigestId) ->
  spawn_link(gossiperl_subscriptions_qos1_redelivery, init, [ Config, Member, EventType, EventObject, ForwardedDigestId ]).

%% @doc Called by start_link.
-spec init( gossiperl_config(), #digestMember{}, binary(), #digestEnvelope{}, binary() ) -> { ok, delivery_reason() }.
init(Config = #overlayConfig{}, Member = #digestMember{}, EventType, EventObject, ForwardedDigestId)
  when is_binary(EventType) andalso is_binary(EventObject)
                            andalso is_binary(ForwardedDigestId) ->
  case gen_server:call( ?SUBSCRIPTIONS( Config ), { register_redelivery, self(), EventType, Member, ForwardedDigestId }) of
    ok ->
      gossiperl_log:info("[~p] Attempting delivery of ~p to ~p.", [Config#overlayConfig.name, ForwardedDigestId, Member#digestMember.member_name]),
      ok = attempt_delivery( Config, Member, EventObject ),
      redelivery( Config, Member, EventType, EventObject, ForwardedDigestId, 0 )
  end.

%% @doc Forwards the message for the delivery to the messaging component of the overlay.
-spec attempt_delivery( gossiperl_config(), #digestMember{}, binary() ) -> ok.
attempt_delivery(Config = #overlayConfig{}, Member = #digestMember{}, EventObject)
  when is_binary(EventObject) ->
  ?MESSAGING( Config ) ! { send_digest, Member, digestEnvelope, EventObject },
  ok.

%% @doc Internal loop.
-spec redelivery( gossiperl_config(), #digestMember{}, binary(), binary(), binary(), non_neg_integer() ) -> { ok, delivery_reason() }.
redelivery( Config = #overlayConfig{}, Member = #digestMember{}, EventType, EventObject, ForwardedDigestId, Attempts )
  when is_binary(EventType) andalso is_binary(EventObject)
                            andalso is_binary(ForwardedDigestId)
                            andalso is_integer(Attempts) ->
  RedeliveryRetryAfter = Config#overlayConfig.redelivery_retry_every * 1000,
  receive
    delivered ->
      gossiperl_log:info("[~p] Digest ~p delivered to ~p.", [Config#overlayConfig.name, ForwardedDigestId, Member#digestMember.member_name]),
      { ok, delivered };
    unsubscribed ->
      gossiperl_log:info("[~p] Digest ~p to ~p delivery cancelled. Subscription ended.", [Config#overlayConfig.name, ForwardedDigestId, Member#digestMember.member_name]),
      { ok, unsubscribed }
  after
    RedeliveryRetryAfter ->

      if
        (Attempts == Config#overlayConfig.redelivery_retry_max) and ( Config#overlayConfig.redelivery_retry_max > 0 ) ->
          gossiperl_log:info("[~p] Digest ~p dropped from redelivery to ~p. Reached max retry attempts (~p).", [Config#overlayConfig.name, ForwardedDigestId, Member#digestMember.member_name, Config#overlayConfig.redelivery_retry_max]),
          ?SUBSCRIPTIONS( Config ) ! { deregister_redelivery, Member, ForwardedDigestId },
          { ok, max_redelivery_attempts };
        true ->
          % redeliver:
          gossiperl_log:info("[~p] Redelivering digest ~p to ~p...", [Config#overlayConfig.name, ForwardedDigestId, Member#digestMember.member_name]),
          ok = attempt_delivery( Config, Member, EventObject ),
          redelivery( Config, Member, EventType, EventObject, ForwardedDigestId, Attempts + 1 )
      end

  end.
