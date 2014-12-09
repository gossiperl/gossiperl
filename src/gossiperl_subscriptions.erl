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

-module(gossiperl_subscriptions).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([list_subscriptions/1, list_shareable_subscriptions/2]).

-include("gossiperl.hrl").

start_link(Config = #overlayConfig{}) ->
  gen_server:start_link({local, ?SUBSCRIPTIONS( Config )}, ?MODULE, [Config], []).

init([Config = #overlayConfig{}]) ->
  subscriptions_store_init(Config),
  check_local_state(Config#overlayConfig.gossip_round_every),
  {ok, {subscriptions, Config}}.

stop() -> gen_server:cast(?MODULE, stop).

handle_cast(stop, LoopData) ->
  {noreply, LoopData}.

handle_call({ register_redelivery, MessageRedeliveryPid, EventType, Member = #digestMember{}, ForwardedDigestId }, From, {subscriptions, Config})
  when is_pid(MessageRedeliveryPid) andalso is_binary(EventType)
                                    andalso is_binary(ForwardedDigestId) ->
  ets:insert( ?ETS_REDELIVERIES( Config ),
              { { ForwardedDigestId, Member#digestMember.member_name, EventType }, MessageRedeliveryPid }),
  gen_server:reply( From, ok ),
  { noreply, {subscriptions, Config} }.

handle_info({ deregister_redelivery, Member = #digestMember{}, ForwardedDigestId }, {subscriptions, Config})
  when is_binary(ForwardedDigestId) ->
  ets:match_delete( ?ETS_REDELIVERIES( Config ),
                    { { ForwardedDigestId, Member#digestMember.member_name, '_' }, '_' }),
  {noreply, {subscriptions, Config}};

handle_info({ update_config, NewConfig = #overlayConfig{} }, {subscriptions, _Config}) ->
  gossiperl_log:notice("[~p] Reconfiguring subscriptions component with ~p.", [ NewConfig#overlayConfig.name, NewConfig ]),
  {noreply, {subscriptions, NewConfig}};

handle_info({subscribe, EventTypes, ClientName, Origin}, {subscriptions, Config})
  when is_list(EventTypes) andalso is_binary(ClientName)
                           andalso is_binary(Origin) ->
  gossiperl_log:info("Subscribing ~p to ~p.", [ ClientName, EventTypes ]),
  subscribe( Config, EventTypes, ClientName, Origin ),
  {noreply, {subscriptions, Config}};

handle_info({unsubscribe, EventTypes, ClientName, Origin}, {subscriptions, Config})
  when is_list(EventTypes) andalso is_binary(ClientName)
                           andalso is_binary(Origin) ->
  gossiperl_log:info("Unsubscribing ~p from ~p.", [ ClientName, EventTypes ]),
  unsubscribe( Config, EventTypes, ClientName, Origin ),
  {noreply, {subscriptions, Config}};

handle_info({notify, EventType, EventObject}, {subscriptions, Config})
  when is_binary(EventType) andalso is_binary(EventObject) ->
  notify_local_subscribers( Config, EventType, EventObject ),
  {noreply, {subscriptions, Config}};

handle_info({notify, EventType, EventObject, {Ip1,Ip2,Ip3,Ip4}, ForwardedDigestId}, {subscriptions, Config})
  when is_binary(EventType) andalso is_binary(EventObject)
                            andalso is_binary(ForwardedDigestId) ->
  notify_remote_subscribers(  Config, EventType, EventObject,
                              list_to_binary( inet:ntoa( {Ip1,Ip2,Ip3,Ip4} ) ),
                              ForwardedDigestId ),
  {noreply, {subscriptions, Config}};

handle_info({cleanup, ClientName}, {subscriptions, Config})
  when is_binary( ClientName ) ->
  ClientSubscriptions = lists:filter( fun( Match ) ->
                                        case Match of [] -> false; [ _, _ ] -> true end
                                      end, ets:match( ?ETS_REDELIVERIES( Config ),
                                                      { {digestSubscription, '$1', ClientName, '$2', '_'} } ) ),
  [ unsubscribe( Config, EventType, ClientName, Origin ) || [ EventType, Origin ] <- ClientSubscriptions ],
  {noreply, {subscriptions, Config}};

handle_info({message_delivered, Member = #digestMember{}, MessageId}, {subscriptions, Config})
  when is_binary( MessageId ) ->
  Redeliveries = ets:match( ?ETS_REDELIVERIES( Config ),
                            { { MessageId, Member#digestMember.member_name, '$1' }, '$2' } ),
  [ begin
      RedeliveryPid ! delivered,
      ets:delete( ?ETS_REDELIVERIES( Config ),
                { MessageId, Member#digestMember.member_name, EventType } )
    end || [ EventType, RedeliveryPid ] <- Redeliveries ],
  {noreply, {subscriptions, Config}};

handle_info({check_local_state}, {subscriptions, Config}) ->
  SelfName = Config#overlayConfig.member_name,
  Timestamp = gossiperl_common:get_timestamp(),
  [ case Subscription#digestSubscription.origin of
      SelfName ->
        % reinsert subscription with new timestamp:
        subscribe(  Config,
                    Subscription#digestSubscription.event_type,
                    Subscription#digestSubscription.member_name,
                    Subscription#digestSubscription.origin,
                    Timestamp );
      _ ->
        if
          Subscription#digestSubscription.heartbeat < Timestamp - Config#overlayConfig.drop_stale_subscriptions_after ->
            unsubscribe( Config,
                         Subscription#digestSubscription.event_type,
                         Subscription#digestSubscription.member_name,
                         Subscription#digestSubscription.origin );
          true ->
            subscribe(  Config,
                        Subscription#digestSubscription.event_type,
                        Subscription#digestSubscription.member_name,
                        Subscription#digestSubscription.origin,
                        Subscription#digestSubscription.heartbeat )
        end
    end || { Subscription } <- list_subscriptions( Config ) ],
  check_local_state(Config#overlayConfig.gossip_round_every),
  {noreply, {subscriptions, Config}};

handle_info({ process_given_list, GivenSubscriptionList }, {subscriptions, Config})
  when is_list( GivenSubscriptionList ) ->

  lists:foldl(fun(GivenSubscription, Acc) ->
    case lists:flatten( ets:match(
      ?ETS_SUBSCRIPTIONS( Config ),
      { { digestSubscription, GivenSubscription#digestSubscription.event_type, GivenSubscription#digestSubscription.member_name, GivenSubscription#digestSubscription.origin, '$1' } }
    ) ) of
      [] ->
        % just subscribe, first time we hear about this one:
        subscribe( Config,
                   GivenSubscription#digestSubscription.event_type,
                   GivenSubscription#digestSubscription.member_name,
                   GivenSubscription#digestSubscription.origin,
                   GivenSubscription#digestSubscription.heartbeat ),
        Acc;
      [ STimestamp ] ->
        if
          GivenSubscription#digestSubscription.heartbeat < STimestamp ->
            % this is older than what we have, do nothing:
            Acc;
          true ->
            % update to the latest:
            subscribe( Config,
                       GivenSubscription#digestSubscription.event_type,
                       GivenSubscription#digestSubscription.member_name,
                       GivenSubscription#digestSubscription.origin,
                       GivenSubscription#digestSubscription.heartbeat ),
            Acc
        end
    end
  end, {noreply, {subscriptions, Config}}, GivenSubscriptionList).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, LoopData) ->
  {ok, LoopData}.

%% doc Initialise ETS storage for an overlay.
-spec subscriptions_store_init( gossiperl_config() ) -> ok.
subscriptions_store_init(Config = #overlayConfig{}) ->
  ets:new( ?ETS_SUBSCRIPTIONS( Config ), [ set, named_table, public ] ),
  ets:new( ?ETS_REDELIVERIES( Config ),  [ set, named_table, public ] ),
  ok.

%% doc Subscribe a client to an event of a specified type.
-spec subscribe
      ( gossiperl_config(), [ binary() ], binary(), binary() ) -> [ true ];
      ( gossiperl_config(), binary(), binary(), binary() ) -> true.
subscribe( Config = #overlayConfig{}, EventTypes, ClientName, Origin )
  when is_list(EventTypes) andalso is_binary(ClientName)
                           andalso is_binary(Origin) ->
  [ subscribe( Config, EventType, ClientName, Origin ) || EventType <- EventTypes ];

subscribe( Config = #overlayConfig{}, EventType, ClientName, Origin )
  when is_binary(EventType) andalso is_binary(ClientName)
                            andalso is_binary(Origin) ->
  subscribe( Config, EventType, ClientName, Origin, gossiperl_common:get_timestamp() ).

-spec subscribe( gossiperl_config(), binary(), binary(), binary(), pos_integer() ) -> true.
subscribe( Config = #overlayConfig{}, EventType, ClientName, Origin, Heartbeat )
  when is_binary(EventType) andalso is_binary(ClientName)
                          andalso is_binary(Origin)
                          andalso is_integer(Heartbeat) ->
  delete_subscription( Config, EventType, ClientName, Origin ),
  ets:insert( ?ETS_SUBSCRIPTIONS( Config ), { #digestSubscription{  event_type=EventType,
                                                                    member_name=ClientName,
                                                                    origin=Origin,
                                                                    heartbeat=Heartbeat } }).

%% doc Unsubscribe a client from an event of a specified type.
-spec unsubscribe
      ( gossiperl_config(), [ binary() ], binary(), binary() ) -> [ true ];
      ( gossiperl_config(), binary(), binary(), binary() ) -> true.
unsubscribe( Config = #overlayConfig{}, EventTypes, ClientName, Origin )
  when is_list(EventTypes) andalso is_binary(ClientName)
                           andalso is_binary(Origin) ->
  [ unsubscribe( Config, EventType, ClientName, Origin ) || EventType <- EventTypes ];

unsubscribe( Config = #overlayConfig{}, EventType, ClientName, Origin )
  when is_binary(EventType) andalso is_binary(ClientName)
                            andalso is_binary(Origin) ->
  delete_subscription( Config, EventType, ClientName, Origin ),
  Redeliveries = ets:match( ?ETS_REDELIVERIES( Config ),
                            { { '$1', ClientName, EventType }, '$2' } ),
  [ begin
      RedeliveryPid ! unsubscribed,
      ets:delete( ?ETS_REDELIVERIES( Config ),
                  { MessageId, ClientName, EventType } )
    end || [ MessageId, RedeliveryPid ] <- Redeliveries ].

%% doc Delete subscription.
-spec delete_subscription( gossiperl_config(), binary(), binary(), binary() ) -> true.
delete_subscription( Config = #overlayConfig{}, EventType, ClientName, Origin )
  when is_binary(EventType) andalso is_binary(ClientName)
                            andalso is_binary(Origin) ->
  ets:match_delete( ?ETS_SUBSCRIPTIONS( Config ),
                    { { digestSubscription, EventType, ClientName, Origin, '_' } }).  

-spec notify_local_subscribers( gossiperl_config(), binary(), binary() ) -> { ok, listeners } | { ok, no_listeners }.
notify_local_subscribers( Config = #overlayConfig{}, EventType, EventObject )
  when is_binary(EventType) andalso is_binary(EventObject) ->
  [ case gen_server:call( ?MEMBERSHIP(Config), { get_member, ClientName } ) of
      { ok, Member } ->
        ?MESSAGING(Config) ! {
          send_digest,
          Member,
          digestEvent,
          #digestEvent{
            event_type = EventType,
            event_object = EventObject,
            heartbeat = gossiperl_common:get_timestamp() } };
      { error, Reason } ->
        gossiperl_log:warn("[~p] Could not notfiy member ~p. Reason ~p.", [ Config#overlayConfig.name, ClientName, Reason ])
    end || ClientName <- list_subscriptions( Config, EventType )].

%% doc Notify subscribers about incoming message for a subscription.
-spec notify_remote_subscribers( gossiperl_config(), binary(), binary(), binary(), binary() ) -> { ok, listeners } | { ok, no_listeners }.
notify_remote_subscribers( Config = #overlayConfig{}, EventType, EventObject, Origin, ForwardedDigestId )
  when is_binary(EventType) andalso is_binary(EventObject)
                            andalso is_binary(Origin)
                            andalso is_binary(ForwardedDigestId) ->
  [ case gen_server:call( ?MEMBERSHIP(Config), { get_member, ClientName } ) of
      { ok, Member } ->
        case Member#digestMember.member_ip of
          Origin ->
            gossiperl_log:warn("[~p] Not forwarding message ~p to the origin (~p).", [ Config#overlayConfig.name, ForwardedDigestId, Member#digestMember.member_ip ]);
          _ ->
            gossiperl_subscriptions_qos1_redelivery:start_link( Config, Member, EventType, EventObject, ForwardedDigestId )
        end;
      { error, Reason } ->
        gossiperl_log:warn("[~p] Could not notfiy member ~p. Reason ~p.", [ Config#overlayConfig.name, ClientName, Reason ])
    end || ClientName <- list_subscriptions( Config, EventType )].

%% doc List registered shareable subscriptions. Excludes member_in, member_out, member_drop, member_quarantine and subscriptions of the member to whom the packet is being sent.
-spec list_shareable_subscriptions( gossiperl_config(), binary() ) -> [ { #digestSubscription{} } ].
list_shareable_subscriptions(Config = #overlayConfig{}, TargetMember) when is_binary( TargetMember ) ->
  lists:foldl(fun({ Subscription }, Acc) ->
    case lists:member( Subscription#digestSubscription.event_type, ?LOCAL_NOTIFICATIONS ) of
      true -> Acc;
      false ->
        SelfName = Config#overlayConfig.member_name,
        case Subscription#digestSubscription.origin of
          SelfName ->
            Acc ++ [ Subscription#digestSubscription{ member_name=SelfName } ];
          _ ->
            case Subscription#digestSubscription.origin of
              TargetMember ->
                Acc; % do not forward subscriptions to subscription owner
              _ ->
                Acc ++ [ Subscription ]
            end
        end
    end
  end, [], list_subscriptions(Config)).

%% doc List registered subscriptions.
-spec list_subscriptions( gossiperl_config() ) -> [ { #digestSubscription{} } ].
list_subscriptions(Config = #overlayConfig{}) ->
  lists:flatten( ets:match( ?ETS_SUBSCRIPTIONS( Config ), '$1' ) ).

%% doc List registered subscriptions of a given event type.
-spec list_subscriptions( gossiperl_config(), binary() ) -> list().
list_subscriptions( Config = #overlayConfig{}, EventType ) when is_binary(EventType) ->
  lists:flatten(ets:match(
    ?ETS_SUBSCRIPTIONS( Config ),
    { { digestSubscription, EventType, '$1', '_', '_'} } ) ).

check_local_state(Interval) ->
  erlang:send_after((Interval * 1000), self(), { check_local_state }).

