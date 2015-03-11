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
      case gen_udp:open(LocalPortToUse, [binary, {ip, {127,0,0,1}}] ++ ?INET_OPTS(Config)) of
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

%% @doc Sends the digest to a member.
handle_info({ send_digest, Member = #digestMember{}, DigestType, Digest }, {messaging, Config}) when is_atom(DigestType) ->
  { ok, SerializedDigest, _ } = gen_server:call( gossiperl_serialization, { serialize, DigestType, Digest }, 5000 ),
  { ok, MaybeEncrypted }      = gen_server:call( ?ENCRYPTION( Config ), { encrypt, SerializedDigest } ),
  gen_server:cast( gossiperl_statistics, { record,
                                           list_to_binary(atom_to_list(Config#overlayConfig.name)),
                                           { data, DigestType, out },
                                           byte_size(MaybeEncrypted) } ),
  gen_udp:send(
    Config#overlayConfig.internal#internalConfig.socket,
    gossiperl_common:parse_binary_ip( Member#digestMember.member_ip ),
    Member#digestMember.member_port,
    MaybeEncrypted ),
  {noreply, {messaging, Config}};

%% @doc Sends the digest over a multicast address.
handle_info({ send_multicast_digest, DigestType, Digest }, {messaging, Config}) when is_atom(DigestType) ->
  { ok, SerializedDigest, _ } = gen_server:call( gossiperl_serialization, { serialize, DigestType, Digest }, 5000 ),
  { ok, MaybeEncrypted }      = gen_server:call( ?ENCRYPTION( Config ), { encrypt, SerializedDigest } ),
  gen_server:cast( gossiperl_statistics, { record,
                                           list_to_binary(atom_to_list(Config#overlayConfig.name)),
                                           { data, DigestType, out },
                                           byte_size(MaybeEncrypted) } ),
  gen_udp:send(
    Config#overlayConfig.internal#internalConfig.socket,
    Config#overlayConfig.multicast#multicastConfig.ip,
    Config#overlayConfig.port,
    MaybeEncrypted ),
  {noreply, {messaging, Config}};

%% RECEIVING

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
              #digestMember{ member_ip = list_to_binary(inet:ntoa(ClientIp)), member_port=ClientPort },
              digestForwardedAck,
              #digestForwardedAck{
                name = Config#overlayConfig.member_name,
                reply_id = ForwardedDigestId,
                secret = Config#overlayConfig.secret } },

  {noreply, {messaging, Config}};

%% @doc Process incoming digest.
handle_info({ message, digest, DecodedPayload = #digest{ name = FromMemberName }, { ClientIp, _ClientPort } }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =/= FromMemberName ->
  % TODO: DecodedPayload#digest.heartbeat - verify that the message is no way to old...
  Member = #digestMember{ member_name = DecodedPayload#digest.name, member_ip = list_to_binary( inet:ntoa( ClientIp ) ),
                          member_port = DecodedPayload#digest.port, member_heartbeat = DecodedPayload#digest.heartbeat },
  gen_server:cast( ?MEMBERSHIP( Config ), { reachable, Member, DecodedPayload#digest.id, DecodedPayload#digest.secret } ),
  {noreply, {messaging, Config}};

%% @doc Process incoming digest.
handle_info({ message, digest, #digest{ name = FromMemberName }, _ }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =:= FromMemberName -> % message from self, ignore
  {noreply, {messaging, Config}};

%% @doc Process incoming digestAck.
handle_info({ message, digestAck, DecodedPayload = #digestAck{ name = FromMemberName }, { ClientIp, _ClientPort } }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =/= FromMemberName ->
  [ gen_server:cast( ?MEMBERSHIP(Config), { reachable_remote,
                                            case Member#digestMember.member_ip of
                                              <<"0.0.0.0">> ->
                                                Member#digestMember{ member_ip = list_to_binary( inet:ntoa( ClientIp ) ) };
                                              _ ->
                                                Member
                                            end } ) || Member <- lists:filter( fun( Member ) ->
                                                                                 Member#digestMember.member_name =/= Config#overlayConfig.member_name
                                                                               end, DecodedPayload#digestAck.membership) ],
  {noreply, {messaging, Config}};

handle_info({ message, digestAck, #digestAck{ name = FromMemberName }, _ }, { messaging, Config = #overlayConfig{ member_name = CurrentMemberName } })
  when CurrentMemberName =:= FromMemberName -> % message from self, ignore
  {noreply, {messaging, Config}};

%% @doc Process incoming digestSubscriptions.
handle_info({ message, digestSubscriptions, DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  ?SUBSCRIPTIONS( Config ) ! { process_given_list, DecodedPayload#digestSubscriptions.subscriptions },
  {noreply, {messaging, Config}};

%% @doc Process incoming digestExit.
handle_info({ message, digestExit, DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DecodedPayload#digestExit.name, DecodedPayload#digestExit.secret } ) of
    { true, _Member } ->
      gen_server:cast(?MEMBERSHIP( Config ), { leave, DecodedPayload#digestExit.name });
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestExit. Secret problematic. Reason: ~p.", [ Config#overlayConfig.name, Reason ])
  end,
  {noreply, {messaging, Config}};

%% @doc Process incoming digestSubscribe.
handle_info({ message, digestSubscribe, DecodedPayload, { {127,0,0,1}, _ClientPort } }, { messaging, Config }) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DecodedPayload#digestSubscribe.name, DecodedPayload#digestSubscribe.secret } ) of
    { true, Member } ->
      ?SUBSCRIPTIONS( Config ) ! { subscribe,
                                   DecodedPayload#digestSubscribe.event_types,
                                   DecodedPayload#digestSubscribe.name,
                                   Config#overlayConfig.member_name },
      DigestSubscribeAck = #digestSubscribeAck{
        heartbeat = gossiperl_common:get_timestamp(),
        reply_id = DecodedPayload#digestSubscribe.id,
        event_types = DecodedPayload#digestSubscribe.event_types },
      self() ! { send_digest, Member, digestSubscribeAck, DigestSubscribeAck };
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestSubscribe. Secret problematic. Reason: ~p.", [ Config#overlayConfig.name, Reason ])
  end,
  {noreply, {messaging, Config}};

handle_info({ message, digestSubscribe, _DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  % digestSubscribe coming from outside of 127.0.0.1, ignoring
  {noreply, {messaging, Config}};

%% @doc Process incoming digestUnsubscribe.
handle_info({ message, digestUnsubscribe, DecodedPayload, { {127,0,0,1}, _ClientPort } }, { messaging, Config }) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DecodedPayload#digestUnsubscribe.name, DecodedPayload#digestUnsubscribe.secret } ) of
    { true, Member } ->
      ?SUBSCRIPTIONS( Config ) ! { unsubscribe,
                                   DecodedPayload#digestUnsubscribe.event_types,
                                   DecodedPayload#digestUnsubscribe.name,
                                   Config#overlayConfig.member_name },
      DigestUnsubscribeAck = #digestUnsubscribeAck{
        heartbeat = gossiperl_common:get_timestamp(),
        reply_id = DecodedPayload#digestUnsubscribe.id,
        event_types = DecodedPayload#digestUnsubscribe.event_types },
      self() ! { send_digest, Member, digestUnsubscribeAck, DigestUnsubscribeAck };
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestUnsubscribe. Secret problematic. Reason: ~p.", [ Config#overlayConfig.name, Reason ])
  end,
  {noreply, {messaging, Config}};

handle_info({ message, digestUnsubscribe, _DecodedPayload, { _ClientIp, _ClientPort } }, { messaging, Config }) ->
  % digestUnsubscribe coming from outside of 127.0.0.1, ignoring
  {noreply, {messaging, Config}};

%% @doc Process incoming digestForwardedAck.
handle_info({ message, digestForwardedAck, DecodedPayload, _Client }, { messaging, Config }) ->
  case gen_server:call( ?MEMBERSHIP(Config), { is_member_secret_valid, DecodedPayload#digestForwardedAck.name, DecodedPayload#digestForwardedAck.secret } ) of
    { true, Member } ->
      ?SUBSCRIPTIONS( Config ) ! { message_delivered, Member, DecodedPayload#digestForwardedAck.reply_id};
    { false, Reason } ->
      gossiperl_log:warn("[~p] Ignoring digestForwardedAck. Secret problematic. Reason: ~p.", [ Config#overlayConfig.name, Reason ])
  end,
  {noreply, {messaging, Config}};

%% ERROR HANDLING

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
