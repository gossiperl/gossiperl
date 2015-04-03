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

-module(gossiperl_messaging).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

-include("gossiperl.hrl").

start_link(Config) ->
  gen_server:start_link({local, ?MESSAGING( Config )}, ?MODULE, [Config], []).

stop() -> gen_server:cast(?MODULE, stop).

init([Config = #overlayConfig{ multicast = undefined, ip = IpAddress, port = Port, name = OverlayName }]) ->
  case gen_udp:open(Port, [binary, {ip, IpAddress}] ++ ?INET_OPTS(Config) ++ ?MULTICAST_OPTS(Config) ) of
    {ok, OverlaySocket} ->
      {ok, {messaging, gossiperl_configuration:overlay_socket( OverlaySocket, Config ) }};
    {error, Reason} ->
      gossiperl_log:err("[~p] Error while starting overlay: ~p", [OverlayName, Reason]),
      {error, Reason}
  end;

init([Config = #overlayConfig{ multicast = #multicastConfig{ local_port = LocalPort }, ip = IpAddress, port = Port, name = OverlayName }]) ->
  case gen_udp:open(Port, [binary, {ip, IpAddress}] ++ ?INET_OPTS(Config) ++ ?MULTICAST_OPTS(Config) ) of
    {ok, OverlaySocket} ->
      LocalPortToUse = case LocalPort of
                         0 -> (Port + 1);
                         _ -> LocalPort
                       end,
      % choose correct local IPvX address based on what kind of address overlay binds to:
      LocalIpAddress = case IpAddress of {_,_,_,_} -> {127,0,0,1}; {_,_,_,_,_,_,_,_} -> {0,0,0,0,0,0,0,1} end,
      case gen_udp:open(LocalPortToUse, [binary, {ip, LocalIpAddress}] ++ ?INET_OPTS(Config)) of
        { ok, LocalOverlaySocket } ->
          {ok, {messaging, gossiperl_configuration:overlay_socket( OverlaySocket, LocalOverlaySocket, Config ) }};
        { error, Reason } ->
          gossiperl_log:err("[~p] Error while starting local socket for a multicast overlay: ~p", [OverlayName, Reason]),
          gen_udp:close( OverlaySocket ),
          {error, Reason}
      end;
    {error, Reason} ->
      gossiperl_log:err("[~p] Error while starting overlay: ~p", [OverlayName, Reason]),
      {error, Reason}
  end.

terminate(Reason, {messaging, #overlayConfig{ multicast = undefined,
                                              name = OverlayName,
                                              internal = #internalConfig{ socket = S } }}) ->
  gossiperl_log:info("[~p] Termination requested (reason ~p). Closing overlay socket.", [ OverlayName, Reason ]),
  gen_udp:close(S);

terminate(Reason, {messaging, #overlayConfig{ multicast = _,
                                              name = OverlayName,
                                              internal = #internalConfig{ socket = S1, local_socket = S2 } }}) ->
  gossiperl_log:info("[~p] Termination requested (reason ~p). Closing overlay and local sockets.", [ OverlayName, Reason ]),
  gen_udp:close(S1), gen_udp:close(S2).

handle_cast(stop, LoopData) ->
  {noreply, LoopData}.

handle_info({ update_config, NewConfig = #overlayConfig{ multicast = undefined,
                                                         name = OverlayName,
                                                         internal = #internalConfig{ socket = S } } }, {messaging, _Config}) ->
  gossiperl_log:notice("[~p] Reconfiguring messaging component with ~p.", [ OverlayName, NewConfig ]),
  inet:setopts(S, ?INET_OPTS(NewConfig) ++ ?MULTICAST_OPTS(NewConfig) ),
  {noreply, {messaging, NewConfig}};

handle_info({ update_config, NewConfig = #overlayConfig{ multicast = _,
                                                         name = OverlayName,
                                                         internal = #internalConfig{ socket = S1,
                                                                                     local_socket = S2 } } }, {messaging, _Config}) ->
  gossiperl_log:notice("[~p] Reconfiguring messaging component with ~p.", [ OverlayName, NewConfig ]),
  inet:setopts(S1, ?INET_OPTS(NewConfig) ++ ?MULTICAST_OPTS(NewConfig) ),
  inet:setopts(S2, ?INET_OPTS(NewConfig) ),
  {noreply, {messaging, NewConfig}};

%% SENDING

%% @doc Sends the digest to a member in a non multicast overlay.
handle_info({ send_digest, #digestMember{ member_ip = MemberIp, member_port = MemberPort }, DigestType, Digest },
            { messaging, Config = #overlayConfig{ name = OverlayName,
                                                  multicast = undefined,
                                                  internal = #internalConfig{ socket = S } } }) when is_atom(DigestType) ->
  _ = deliver_digest_via_socket( DigestType, Digest, OverlayName, ?ENCRYPTION( Config ),
                                 gossiperl_common:parse_binary_ip( MemberIp ), MemberPort, S ),
  {noreply, {messaging, Config}};

%% @doc Sends the digest to a member in a multicast overlay.
handle_info({ send_digest, #digestMember{ member_ip = MemberIp }, DigestType, Digest },
            { messaging, Config = #overlayConfig{ name = OverlayName,
                                                  port = OverlayPort,
                                                  multicast = #multicastConfig{ ip = MulticastIp },
                                                  internal = #internalConfig{ socket = S } } })
  when is_atom(DigestType) andalso MemberIp =/= <<"127.0.0.1">> andalso MemberIp =/= <<"::1">> ->
  _ = deliver_digest_via_socket( DigestType, Digest, OverlayName, ?ENCRYPTION( Config ),
                                 MulticastIp, OverlayPort, S ),
  {noreply, {messaging, Config}};

%% @doc Sends the digest to a local member when in multicast overlay.
handle_info({ send_digest, #digestMember{ member_ip = MemberIp, member_port = MemberPort }, DigestType, Digest },
            { messaging, Config = #overlayConfig{ name = OverlayName,
                                                  multicast = #multicastConfig{},
                                                  internal = #internalConfig{ local_socket = S } } })
  when is_atom(DigestType) andalso ( MemberIp =:= <<"127.0.0.1">> orelse MemberIp =:= <<"::1">> ) ->
  { ok, Address } = inet:parse_address( binary_to_list( MemberIp ) ),
  _ = deliver_digest_via_socket( DigestType, Digest, OverlayName, ?ENCRYPTION( Config ),
                                 Address, MemberPort, S ),
  {noreply, {messaging, Config}};

%% @doc Sends the digest over a multicast address.
handle_info({ send_multicast_digest, DigestType, Digest },
            { messaging, Config = #overlayConfig{ name = OverlayName,
                                                  port = OverlayPort,
                                                  internal = #internalConfig{ socket = S },
                                                  multicast = #multicastConfig{ ip = MulticastIp } } }) when is_atom(DigestType) ->
  _ = deliver_digest_via_socket( DigestType, Digest, OverlayName, ?ENCRYPTION( Config ),
                                 MulticastIp, OverlayPort, S ),
  {noreply, {messaging, Config}};

%%% RECEIVING

%% @doc gen_udp callback, initializes receiving process by passing the message for decryption.
handle_info({udp, _ClientSocket, ClientIp, ClientPort, Msg}, {messaging, Config}) ->
  case gen_server:call( ?ENCRYPTION( Config ), { decrypt, Msg } ) of
    { ok, MaybeDecrypted } ->
      % deserialize:
      DeserializeResult = gen_server:call( gossiperl_serialization, { deserialize, MaybeDecrypted } ),
      self() ! { message_deserialized, DeserializeResult, { ClientIp, ClientPort, Msg } };
    { error, Reason } ->
      gen_server:cast( gossiperl_statistics, { record,
                                               list_to_binary(atom_to_list(Config#overlayConfig.name)),
                                               { message_failed, decrypt, in },
                                               1 } ),
      gossiperl_log:err("[~p] Message decrypt failed. Reason ~p.", [Config#overlayConfig.name, Reason])
  end,
  ?ENCRYPTION( Config ) ! { decrypt, Msg, self(), { ClientIp, ClientPort, Msg } },
  {noreply, {messaging, Config}};

%% @doc Called by the serialization module after successful deserialization.
handle_info({ message_deserialized, { ok, DecodedPayloadType, DecodedPayload }, State }, {messaging, Config}) ->
  { ClientIp, ClientPort, OriginalMessage } = State,
  gen_server:cast( gossiperl_statistics, { record,
                                           list_to_binary(atom_to_list(Config#overlayConfig.name)),
                                           { data, DecodedPayloadType, in },
                                           byte_size(OriginalMessage) } ),
  self() ! { message, DecodedPayloadType, DecodedPayload, { ClientIp, ClientPort } },
  {noreply, {messaging, Config}};

%% @doc Processing a digest type not recognised by gossiperl. These are forwardable messages. Notifies subscribers and replies with digestForwardedAck.
handle_info({ message_deserialized, {forwardable, ForwardedMessageType, DigestEnvelopeBinary, ForwardedDigestId }, State }, {messaging, Config})
  when is_binary(ForwardedMessageType) andalso is_binary(DigestEnvelopeBinary)
                                       andalso is_binary(ForwardedDigestId) ->
  { ClientIp, ClientPort, _OriginalMessage } = State,
  gen_server:cast( gossiperl_statistics, { record,
                                           list_to_binary(atom_to_list(Config#overlayConfig.name)),
                                           { data, ForwardedMessageType, in },
                                           byte_size(DigestEnvelopeBinary) } ),
  ?SUBSCRIPTIONS( Config ) ! { notify, ForwardedMessageType, DigestEnvelopeBinary, ClientIp, ForwardedDigestId },
  gossiperl_log:info("[~p] sending digestForwardedAck to: ~p:~p", [ Config#overlayConfig.name, ClientIp, ClientPort ]),
  self() ! {  send_digest,
              #digestMember{ member_ip = gossiperl_common:ip_to_binary(ClientIp), member_port=ClientPort },
              digestForwardedAck,
              #digestForwardedAck{
                name = Config#overlayConfig.member_name,
                reply_id = ForwardedDigestId,
                secret = Config#overlayConfig.secret } },

  {noreply, {messaging, Config}};

%%% DIGEST

%% @doc Process incoming digest.
handle_info({ message, digest, DecodedPayload = #digest{ name = FromMemberName }, { ClientIp, _ClientPort } }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =/= FromMemberName ->
  % TODO: DecodedPayload#digest.heartbeat - verify that the message is no way to old...
  Member = #digestMember{ member_name = DecodedPayload#digest.name, member_ip = gossiperl_common:ip_to_binary( ClientIp ),
                          member_port = DecodedPayload#digest.port, member_heartbeat = DecodedPayload#digest.heartbeat },
  gen_server:cast( ?MEMBERSHIP( Config ), { reachable, Member, DecodedPayload#digest.id, DecodedPayload#digest.secret } ),
  {noreply, {messaging, Config}};

%% @doc Process incoming digest.
handle_info({ message, digest, #digest{ name = FromMemberName }, _ }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =:= FromMemberName -> % message from self, ignore
  {noreply, {messaging, Config}};

%%% DIGEST_ACK

%% @doc Process incoming digestAck.
handle_info({ message, digestAck, DecodedPayload = #digestAck{ name = FromMemberName }, { ClientIp, _ClientPort } }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =/= FromMemberName ->
  [ gen_server:cast( ?MEMBERSHIP(Config), { reachable_remote,
                                            case Member#digestMember.member_ip of
                                              <<"0.0.0.0">> -> Member#digestMember{ member_ip = gossiperl_common:ip_to_binary( ClientIp ) };
                                              <<"::">>      -> Member#digestMember{ member_ip = gossiperl_common:ip_to_binary( ClientIp ) };
                                              _             -> Member
                                            end } ) || Member <- lists:filter( fun( Member ) ->
                                                                                 Member#digestMember.member_name =/= Config#overlayConfig.member_name
                                                                               end, DecodedPayload#digestAck.membership) ],
  {noreply, {messaging, Config}};

handle_info({ message, digestAck, #digestAck{ name = FromMemberName }, _ }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =:= FromMemberName -> % message from self, ignore
  {noreply, {messaging, Config}};

%%% DIGEST_SUBSCRIPTIONS

%% @doc Process incoming digestSubscriptions.
handle_info({ message, digestSubscriptions, DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  ?SUBSCRIPTIONS( Config ) ! { process_given_list, DecodedPayload#digestSubscriptions.subscriptions },
  {noreply, {messaging, Config}};

%%% DIGEST_EXIT

%% @doc Process incoming digestExit.
handle_info({ message, digestExit, DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DecodedPayload#digestExit.name, DecodedPayload#digestExit.secret } ) of
    { true, _Member } ->
      gen_server:cast(?MEMBERSHIP( Config ), { leave, DecodedPayload#digestExit.name });
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestExit. Secret problematic. Reason: ~p.", [ Config#overlayConfig.name, Reason ])
  end,
  {noreply, {messaging, Config}};

%%% DIGEST_SUBSCRIBE

%% @doc Process incoming digestSubscribe.
handle_info({ message, digestSubscribe, DecodedPayload, { {0,0,0,0,0,0,0,1}, _ClientPort } }, { messaging, Config }) ->
  {noreply, {messaging, process_digest_subscribe( DecodedPayload, Config )}};

handle_info({ message, digestSubscribe, DecodedPayload, { {127,0,0,1}, _ClientPort } }, { messaging, Config }) ->
  {noreply, {messaging, process_digest_subscribe( DecodedPayload, Config )}};

handle_info({ message, digestSubscribe, _DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  % digestSubscribe coming from outside of localhost, ignoring
  {noreply, {messaging, Config}};

%%% DIGEST_UNSUBSCRIBE

%% @doc Process incoming digestUnsubscribe.
handle_info({ message, digestUnsubscribe, DecodedPayload, { {0,0,0,0,0,0,0,1}, _ClientPort } }, { messaging, Config }) ->
  {noreply, {messaging, process_digest_unsubscribe( DecodedPayload, Config )}};

handle_info({ message, digestUnsubscribe, DecodedPayload, { {127,0,0,1}, _ClientPort } }, { messaging, Config }) ->
  {noreply, {messaging, process_digest_unsubscribe( DecodedPayload, Config )}};

handle_info({ message, digestUnsubscribe, _DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  % digestUnsubscribe coming from outside of localhost, ignoring
  {noreply, {messaging, Config}};

%%% DIGEST_FORWARDED_ACK

%% @doc Process incoming digestForwardedAck.
handle_info({ message, digestForwardedAck, DecodedPayload, _Client }, { messaging, Config }) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DecodedPayload#digestForwardedAck.name, DecodedPayload#digestForwardedAck.secret } ) of
    { true, Member } ->
      ?SUBSCRIPTIONS( Config ) ! { message_delivered, Member, DecodedPayload#digestForwardedAck.reply_id};
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestForwardedAck. Secret problematic. Reason: ~p.", [ Config#overlayConfig.name, Reason ])
  end,
  {noreply, {messaging, Config}};

%%% ERROR HANDLING

%% @doc Handle message deserialization error.
handle_info({ message_deserialized, {error, Reason}, _ }, {messaging, Config}) ->
  gen_server:cast( gossiperl_statistics, { record,
                                           list_to_binary(atom_to_list(Config#overlayConfig.name)),
                                           { message_failed, decode, in },
                                           1 } ),
  gossiperl_log:warn("[~p] Message decode failed. Reason ~p.", [Config#overlayConfig.name, Reason]),
  {noreply, {messaging, Config}}.

handle_call( stop, _From, { messaging, Config }) ->
  gossiperl_log:info("[~p] Stop requested. Closing sockets.", [ Config#overlayConfig.name ]),
  gen_udp:close(Config#overlayConfig.internal#internalConfig.socket),
  { reply, ok, { messaging, Config } }.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc Delivers digest to the destination.
-spec deliver_digest_via_socket( atom(), any(), atom(), pid(), inet:ip_address(), integer(), port() ) -> ok | { error, term() }.
deliver_digest_via_socket( DigestType, Digest, OverlayName, Encryption, Ip, Port, Socket ) ->
  { ok, SerializedDigest, _ } = gen_server:call( gossiperl_serialization, { serialize, DigestType, Digest } ),
  { ok, MaybeEncrypted }      = gen_server:call( Encryption, { encrypt, SerializedDigest } ),
  gen_server:cast( gossiperl_statistics, { record,
                                           list_to_binary(atom_to_list(OverlayName)),
                                           { data, DigestType, out },
                                           byte_size(MaybeEncrypted) } ),
  gen_udp:send( Socket, Ip, Port, MaybeEncrypted ).

process_digest_subscribe( #digestSubscribe{ name = DigestMemberName, secret = Secret, event_types = EventTypes, id = DigestId },
                          Config = #overlayConfig{ name = OverlayName, member_name = MemberName } ) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DigestMemberName, Secret } ) of
    { true, Member } ->
      ?SUBSCRIPTIONS( Config ) ! { subscribe, EventTypes, DigestMemberName, MemberName },
      self() ! { send_digest, Member, digestSubscribeAck,
                 #digestSubscribeAck{ heartbeat = gossiperl_common:get_timestamp(), reply_id = DigestId, event_types = EventTypes } };
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestSubscribe. Secret problematic. Reason: ~p.", [ OverlayName, Reason ])
  end,
  Config.

process_digest_unsubscribe( #digestUnsubscribe{ name = DigestMemberName, secret = Secret, event_types = EventTypes, id = DigestId },
                            Config = #overlayConfig{ name = OverlayName, member_name = MemberName } ) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DigestMemberName, Secret } ) of
    { true, Member } ->
      ?SUBSCRIPTIONS( Config ) ! { unsubscribe, EventTypes, DigestMemberName, MemberName },
      self() ! { send_digest, Member, digestUnsubscribeAck,
                 #digestUnsubscribeAck{ heartbeat = gossiperl_common:get_timestamp(), reply_id = DigestId, event_types = EventTypes } };
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestUnsubscribe. Secret problematic. Reason: ~p.", [ OverlayName, Reason ])
  end,
  Config.
