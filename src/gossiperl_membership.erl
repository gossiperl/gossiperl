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

-module(gossiperl_membership).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("gossiperl.hrl").

start_link(Config = #overlayConfig{}) ->
  gen_server:start_link({local, ?MEMBERSHIP(Config)}, ?MODULE, [Config], []).

stop() -> gen_server:cast(?MODULE, stop).

init([Config = #overlayConfig{}]) ->
  process_flag(trap_exit, true),
  erlang:send_after(
    (Config#overlayConfig.gossip_round_every * 1000),
    ?MEMBERSHIP(Config),
    { gossip }),
  {ok, {membership, Config, dict:new()}}.

%% @doc Handle digest given by messaging.
handle_cast({ reachable, Member=#digestMember{ member_name=MemberName, member_ip=MemberIp, member_port=MemberPort }, DigestId, Secret },
            { membership, Config=#overlayConfig{ name=OverlayName }, Membership }) when MemberIp =:= <<"127.0.0.1">> ->
  case dict:is_key(MemberName, Membership) of
    false ->
      {ok, MemberFsmPid} = gossiperl_member_fsm:start_link( Config, MemberName, MemberIp, MemberPort, gossiperl_common:get_timestamp(), Secret ),
      gen_server:cast( self(), { digest_ack, Member, DigestId } ),
      {noreply, {membership, Config, dict:store(MemberName, MemberFsmPid, Membership)}};
    true ->
      try
        MemberFsmPid = dict:fetch(MemberName, Membership),
        case gen_fsm:sync_send_all_state_event(MemberFsmPid, { is_secret_valid, Secret }) of
          true ->
            gen_fsm:send_event( MemberFsmPid, { reachable, gossiperl_common:get_timestamp() } ),
            gen_server:cast( self(), { digest_ack, Member, DigestId } );
          false ->
            gossiperl_log:warn("[~p] Invalid secret for member ~p. Ignore digest.", [ Config#overlayConfig.name, MemberName ])
        end
      catch Error:Reason ->
        gossiperl_log:err("[~p] Could not update reachability state for ~p. Reason: ~p:~p.", [ OverlayName, MemberName, Error, Reason ])
      end,
      {noreply, {membership, Config, Membership}}
  end;

handle_cast({ reachable, Member=#digestMember{ member_name=MemberName, member_ip=MemberIp, member_port=MemberPort }, DigestId, Secret },
            { membership, Config=#overlayConfig{ name=OverlayName }, Membership }) when MemberIp =/= <<"127.0.0.1">> ->
  case Config#overlayConfig.secret of
    Secret ->
      case dict:is_key(MemberName, Membership) of
        false ->
          {ok, MemberFsmPid} = gossiperl_member_fsm:start_link( Config, MemberName, MemberIp, MemberPort, gossiperl_common:get_timestamp(), Secret ),
          gen_server:cast( self(), { digest_ack, Member, DigestId } ),
          {noreply, {membership, Config, dict:store(MemberName, MemberFsmPid, Membership)}};
        true ->
          try
            MemberFsmPid = dict:fetch(MemberName, Membership),
            case gen_fsm:sync_send_all_state_event(MemberFsmPid, { is_secret_valid, Secret }) of
              true ->
                gen_fsm:send_event( MemberFsmPid, { reachable, gossiperl_common:get_timestamp() } ),
                gen_server:cast( self(), { digest_ack, Member, DigestId } );
              false ->
                gossiperl_log:warn("[~p] Member ~p sent wrong secret, message ignored.", [ Config#overlayConfig.name, MemberName ])
            end
          catch Error:Reason ->
            gossiperl_log:err("[~p] Could not update reachability state for ~p. Reason: ~p:~p.", [ OverlayName, MemberName, Error, Reason ])
          end,
          {noreply, {membership, Config, Membership}}
      end;
    _ ->
      gossiperl_log:warn("[~p] Member ~p sent invalid secret. Ignore digest.", [ Config#overlayConfig.name, MemberName ]),
      {noreply, {membership, Config, Membership}}
  end;

handle_cast({ reachable_remote, #digestMember{ member_name=MemberName, member_ip=MemberIp, member_port=MemberPort, member_heartbeat=MemberHeartbeat }},
            { membership, Config=#overlayConfig{ name=OverlayName }, Membership }) ->
  case dict:is_key(MemberName, Membership) of
    false ->
      {ok, MemberFsmPid} = gossiperl_member_fsm:start_link( Config, MemberName, MemberIp, MemberPort, MemberHeartbeat, Config#overlayConfig.secret ),
      {noreply, {membership, Config, dict:store(MemberName, MemberFsmPid, Membership)}};
    true ->
      try
        gen_fsm:send_event( dict:fetch(MemberName, Membership), { reachable, MemberHeartbeat } )
      catch
        Error:Reason ->
          gossiperl_log:error("[~p] Could not update reachability state for ~p. Reason: ~p:~p.", [ OverlayName, MemberName, Error, Reason ])
      end,
      {noreply, {membership, Config, Membership}}
  end;

handle_cast({ digest_ack, Member=#digestMember{ member_ip=MemberIp }, DigestId }, {membership, Config, Membership}) ->
  AllMembers = [ self_as_member(Config) ] ++ to_digest_members(
                                               list_members( reachable,
                                                             case MemberIp of <<"127.0.0.1">> -> local; _ -> remote end,
                                                             dict:to_list(Membership) ) ),
  PacketDigestAck = #digestAck{
    name = Config#overlayConfig.member_name,
    heartbeat = gossiperl_common:get_timestamp(),
    reply_id = DigestId,
    membership = AllMembers },
  ?MESSAGING( Config ) ! { send_digest, Member, digestAck, PacketDigestAck },
  % These 2 are split because we don't want to put too much stuff in a single UDP packet:
  gen_server:cast(self(), { digest_subscriptions, Member, DigestId }),
  {noreply, {membership, Config, Membership}};

handle_cast({ digest_subscriptions, #digestMember{ member_ip = <<"127.0.0.1">> }, _DigestId }, {membership, Config, Membership}) ->
  {noreply, {membership, Config, Membership}};

handle_cast({ digest_subscriptions, Member = #digestMember{ member_name = MemberName }, DigestId }, {membership, Config, Membership}) ->
  Subscriptions = gossiperl_subscriptions:list_shareable_subscriptions( Config, MemberName ),
  gen_server:cast(self(), { digest_subscriptions, Member, DigestId, Subscriptions }),
  {noreply, {membership, Config, Membership}};

handle_cast({ digest_subscriptions, _, _, Subscriptions }, {membership, Config, Membership})
  when is_list(Subscriptions) andalso length(Subscriptions) =:= 0 ->
  {noreply, {membership, Config, Membership}};

handle_cast({ digest_subscriptions, Member=#digestMember{}, DigestId, Subscriptions }, {membership, Config, Membership})
  when is_list(Subscriptions) andalso length(Subscriptions) > 0 ->
  PacketSubs = #digestSubscriptions{
    name = Config#overlayConfig.member_name,
    heartbeat = gossiperl_common:get_timestamp(),
    reply_id = DigestId,
    subscriptions = Subscriptions },
  ?MESSAGING( Config ) ! { send_digest, Member, digestSubscriptions, PacketSubs },
  {noreply, {membership, Config, Membership}};

handle_cast({ leave, MemberName }, {membership, Config, Membership}) ->
  try
    ok = gen_fsm:sync_send_all_state_event( dict:fetch(MemberName, Membership), { leave } )
  catch
    Error:Reason ->
      gossiperl_log:error("[~p] Could not pass leave message to the member ~p. Reason: ~p:~p.", [ Config#overlayConfig.name, MemberName, Error, Reason ])
  end,
  {noreply, {membership, Config, Membership}};

handle_cast({ member_state_change, BinState, MemberName }, { membership, Config, Membership }) ->
  gossiperl_log:info("[~p][~p] Changing status to ~p. Notifing subscribers.", [ Config#overlayConfig.name, MemberName, BinState ]),
  ?SUBSCRIPTIONS(Config) ! { notify, BinState, MemberName },
  {noreply, {membership, Config, Membership}};

handle_cast(stop, LoopData) ->
  {noreply, LoopData}.

handle_info({'EXIT', _MemberPid, {dropped, MemberName, MemberIp}}, {membership, Config, Membership}) ->
  ?SUBSCRIPTIONS( Config ) ! { notify, <<"member_out">>, MemberName },
  case is_seed( MemberIp, Config#overlayConfig.seeds ) of
    true ->
      gossiperl_log:warn("[~p] Not dropping a seed: ~p", [ Config#overlayConfig.name, MemberName ]),
      {noreply, {membership, Config, Membership}};
    false ->
      {noreply, {membership, Config, dict:erase(MemberName, Membership)}}
  end;

handle_info({ update_config, NewConfig = #overlayConfig{} }, {membership, _Config, Membership}) ->
  gossiperl_log:info("[~p] Reconfiguring membership component with ~p.", [ NewConfig#overlayConfig.name, NewConfig ]),
  {noreply, {membership, NewConfig, Membership}};

handle_info({ gossip }, {membership, Config, Membership}) ->
  
  erlang:send_after(
    (Config#overlayConfig.gossip_round_every * 1000),
    ?MEMBERSHIP( Config ), { gossip }),
  
  Digest = #digest{
    name = Config#overlayConfig.member_name,
    port = Config#overlayConfig.port,
    heartbeat = gossiperl_common:get_timestamp(),
    id = uuid:uuid_to_string(uuid:get_v4()),
    secret = Config#overlayConfig.secret },
  
  case Config#overlayConfig.multicast of
    undefined ->
      case gossip_reachable( remote, digest, Digest, Config, dict:to_list(Membership) ) of
        undefined ->
          gossip_seed( digest, Digest, Config ),
          gossip_unreachable( remote, digest, Digest, Config, dict:to_list(Membership) );
        _ ->
          gossip_unreachable( remote, digest, Digest, Config, dict:to_list(Membership) )
      end;
    _ ->
      ?MESSAGING( Config ) ! { send_multicast_digest, digest, Digest }
  end,
  
  {noreply, {membership, Config, Membership}}.

handle_call({ get_member, MemberName }, From, { membership, Config, Membership }) ->
  try
    gen_server:reply( From, { ok, to_digest_member( gen_fsm:sync_send_all_state_event( dict:fetch( MemberName, Membership ), { member_info } ) ) } )
  catch
    Error:Reason -> gen_server:reply( From, { error, { Error, Reason } } )
  end,
  {noreply, {membership, Config, Membership}};

handle_call({ is_member_secret_valid, MemberName, GivenSecret }, From, { membership, Config, Membership }) ->
  try
    MemberFsmPid = dict:fetch( MemberName, Membership ),
    case gen_fsm:sync_send_all_state_event( MemberFsmPid, { is_secret_valid, GivenSecret } ) of
      true ->
        gen_server:reply( From, { true, to_digest_member( gen_fsm:sync_send_all_state_event( MemberFsmPid, { member_info } ) ) } );
      false ->
        gen_server:reply( From, { false, invalid } )
    end
  catch
    Error:Reason -> gen_server:reply( From, { false, { Error, Reason } } )
  end,
  {noreply, {membership, Config, Membership}};

handle_call(list_members_with_state, From, {membership, Config, Membership}) ->
  gen_server:reply( From, [ { remote, reachable, self_as_member(Config) } ] ++ to_digest_members(
                                                                                 list_members( dict:to_list(Membership) ), details ) ),
  {noreply, {membership, Config, Membership}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, LoopData) ->
  {ok, LoopData}.

%% GOSSIPPING FUNCTIONS
%% --------------------

%% @doc Gossip digest to a random unreachable member.
-spec gossip_unreachable( atom(), atom(), term(), gossiperl_config(), list() ) -> #digestMember{} | undefined.
gossip_unreachable(Visibility, DigestType, Digest, Config = #overlayConfig{}, MembershipList)
  when is_atom(Visibility) andalso is_atom(DigestType)
                           andalso is_list(MembershipList) ->
  case random_member(unreachable, Visibility, MembershipList) of
    undefined ->
      undefined;
    Member ->
      ?MESSAGING( Config ) ! { send_digest, Member, DigestType, Digest },
      Member
  end.

%% @doc Gossip digest to a random reachable member.
-spec gossip_reachable( atom(), atom(), term(), gossiperl_config(), list() ) -> #digestMember{} | undefined.
gossip_reachable(Visibility, DigestType, Digest, Config = #overlayConfig{}, MembershipList)
  when is_atom(Visibility) andalso is_atom(DigestType)
                           andalso is_list(MembershipList) ->
  case random_member(reachable, Visibility, MembershipList) of
    undefined ->
      undefined;
    Member ->
      ?MESSAGING( Config ) ! { send_digest, Member, DigestType, Digest },
      Member
  end.

%% @doc Gossip digest to a random seed.
-spec gossip_seed( atom(), term(), gossiperl_config() ) -> inet:ip_address() | undefined.
gossip_seed(DigestType, Digest, Config = #overlayConfig{}) when is_atom(DigestType) ->
  case random_seed(Config) of
    undefined ->
      undefined;
    SeedIp ->
      ?MESSAGING( Config ) ! { send_digest, #digestMember{ member_ip=gossiperl_common:ip_to_binary(SeedIp), member_port=Config#overlayConfig.port }, DigestType, Digest },
      SeedIp
  end.

%% MEMBERSHIP FUNCTIONS
%% --------------------

%% @doc Get member for current overlay.
-spec self_as_member( gossiperl_config() ) -> #digestMember{}.
self_as_member( Config = #overlayConfig{} ) ->
  #digestMember{
    member_name      = Config#overlayConfig.member_name,
    member_ip        = gossiperl_common:ip_to_binary(Config#overlayConfig.ip),
    member_port      = Config#overlayConfig.port,
    member_heartbeat = gossiperl_common:get_timestamp() }.

%% @doc Get random member from a list of members.
-spec random_member(atom(), atom(), list()) -> #digestMember{} | undefined.
random_member(Status, Visibility, MembershipList)
  when is_atom(Visibility) andalso is_atom(Status)
                           andalso is_list(MembershipList) ->
  MembersInfo = list_members(Status, Visibility, MembershipList),
  case MembersInfo of
    [] -> undefined;
    [ _ | _ ] ->
      to_digest_member( lists:nth( random:uniform( length( MembersInfo ) ), MembersInfo ) )
  end.

-spec random_seed(gossiperl_config()) -> #digestMember{} | undefined.
random_seed(Config = #overlayConfig{}) ->
  SeedIps = remove_known_ips( Config#overlayConfig.seeds, [ Config#overlayConfig.ip_hint ] ++ Config#overlayConfig.internal#internalConfig.knownIps ),
  case SeedIps of [] -> undefined; _ -> lists:nth( random:uniform( length( SeedIps ) ), SeedIps ) end.

%% @doc Is member a seed?
-spec is_seed( binary(), [ inet:ip_address() ] ) -> boolean().
is_seed(MemberIp, Seeds) when is_binary(MemberIp) andalso is_list(Seeds) ->
  lists:member( gossiperl_common:parse_binary_ip( MemberIp ), Seeds ).

%% @doc Substract known IP addresses from the list of IP addresses. Used for seed establishing.
-spec remove_known_ips( [ inet:ip_address() ], [ inet:ip_address() ] ) -> [ inet:ip_address() ].
remove_known_ips( ListOfIps, KnownIps )
  when is_list(ListOfIps) andalso is_list(KnownIps) ->
  ListOfIps -- KnownIps.

to_digest_member_with_details({Reachability,Status,MemberName,MemberIp,MemberPort,MemberHeartbeat}) ->
  { Reachability, Status, #digestMember{ member_name=MemberName, member_ip=MemberIp, member_port=MemberPort, member_heartbeat=MemberHeartbeat } }.

to_digest_member({_,_,MemberName,MemberIp,MemberPort,MemberHeartbeat}) ->
  #digestMember{ member_name=MemberName, member_ip=MemberIp, member_port=MemberPort, member_heartbeat=MemberHeartbeat }.

to_digest_members(MembersInfo) ->
  [ to_digest_member( MemberInfo ) || MemberInfo <- MembersInfo ].

to_digest_members(MembersInfo, details) ->
  [ to_digest_member_with_details( MemberInfo ) || MemberInfo <- MembersInfo ].

-spec list_members([ { binary(), pid() } ]) -> [ { atom(), atom(), binary(), binary(), integer(), integer() } ].
list_members(MembershipList) when is_list(MembershipList) ->
  [ gen_fsm:sync_send_all_state_event(Pid, { member_info }) || { _, Pid } <- MembershipList ].

-spec list_members(atom(), atom(), [ { binary(), pid() } ]) -> [ { atom(), atom(), binary(), binary(), integer(), integer() } ].
list_members(Status, Reachability, MembershipList) when is_atom(Status) andalso is_atom(Reachability)
                                                                        andalso is_list(MembershipList) ->
  lists:filter(fun({ StoredReachability, StoredStatus, _, _, _, _ }) ->
    { StoredStatus, StoredReachability } =:= { Status, Reachability }
  end, list_members(MembershipList)).
