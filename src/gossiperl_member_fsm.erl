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

-module(gossiperl_member_fsm).

-behaviour(gen_fsm).

-include("gossiperl.hrl").

-export([start_link/6, init/1, handle_info/3, terminate/3, code_change/4, handle_sync_event/4, handle_event/3]).
-export([reachable/2, unreachable/2, quarantine/2, dropped/2]).

-record(state, {
          name :: binary(),
          ip :: binary(),
          port :: integer(),
          quarantine_after :: integer(),
          max_quarantined :: integer(),
          drop_unreachable_after :: integer(),
          secret :: binary(),
          last_heard :: integer(),
          membership :: pid() }).

start_link(Config, MemberName, Ip, Port, Heartbeat, Secret) ->
  gen_fsm:start_link(?MODULE, [Config, MemberName, Ip, Port, Heartbeat, Secret], []).

init([Config, MemberName, Ip, Port, Heartbeat, Secret]) ->
  gossiperl_log:info("[MEMBER][~p] Added as new.", [ MemberName ]),
  State = #state{ name = MemberName, ip = Ip, port = Port, secret = Secret,
                  quarantine_after = Config#overlayConfig.quarantine_after,
                  max_quarantined = Config#overlayConfig.max_quarantined,
                  drop_unreachable_after = Config#overlayConfig.drop_unreachable_after,
                  last_heard = Heartbeat,
                  membership = ?MEMBERSHIP(Config) },
  gen_server:cast( ?MEMBERSHIP(Config), { member_state_change, <<"member_in">>, MemberName } ),
  { ok, reachable, State, ?MEMBER_CHECK_STATE_EVERY }.

handle_info(_AnyInfo, State, LoopData) ->
  { next_state, State, LoopData }.

handle_event(stop, _StateName, StateData) ->
  {stop, normal, StateData}.

handle_sync_event({ leave }, From, _State, S) ->
  gen_fsm:reply(From, ok),
  { next_state, dropped, S, 0 };

handle_sync_event({ member_info }, From, State, S=#state{ name=MemberName, ip=Ip, port=Port, last_heard=LastHeardOf }) ->
  gen_fsm:reply(From, { case Ip of <<"127.0.0.1">> -> local; _ -> remote end, State, MemberName, Ip, Port, LastHeardOf }),
  { next_state, State, S, ?MEMBER_CHECK_STATE_EVERY };

handle_sync_event({ is_secret_valid, OtherSecret }, From, State, S=#state{ secret=Secret }) ->
  gen_fsm:reply(From, ( OtherSecret =:= Secret )),
  { next_state, State, S, ?MEMBER_CHECK_STATE_EVERY };

handle_sync_event(_Msg, _From, State, LoopData) ->
  { next_state, State, LoopData, ?MEMBER_CHECK_STATE_EVERY }.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(_Reason, _State, _LoopData) ->
  {ok}.

reachable(timeout, S=#state{ quarantine_after=QuarantineAfter, last_heard=LastHeardOf, name=MemberName, membership = MembershipPid }) ->
  Timestamp = gossiperl_common:get_timestamp(),
  if
    ( Timestamp - LastHeardOf >= QuarantineAfter ) ->
      gen_server:cast( MembershipPid, { member_state_change, <<"member_quarantine">>, MemberName } ),
      { next_state, quarantine, S, 0 };
    true ->
      { next_state, reachable, S, ?MEMBER_CHECK_STATE_EVERY }
  end;

reachable({reachable, Heartbeat}, S=#state{}) ->
  { next_state, reachable, S#state{ last_heard=Heartbeat }, ?MEMBER_CHECK_STATE_EVERY }.

quarantine(timeout, S=#state{ quarantine_after=QuarantineAfter, max_quarantined=MaxQuarantined, name=MemberName, last_heard=LastHeardOf, membership = MembershipPid }) ->
  Timestamp = gossiperl_common:get_timestamp(),
  if
    ( Timestamp - LastHeardOf >= ( QuarantineAfter + MaxQuarantined ) ) ->
      gen_server:cast( MembershipPid, { member_state_change, <<"member_unreachable">>, MemberName } ),
      { next_state, unreachable, S, 0 };
    true ->
      { next_state, quarantine, S, ?MEMBER_CHECK_STATE_EVERY }
  end;

quarantine({reachable, Heartbeat}, S=#state{ name=MemberName, membership=MembershipPid }) ->
  gen_server:cast( MembershipPid, { member_state_change, <<"member_rejoin">>, MemberName } ),
  { next_state, reachable, S#state{ last_heard=Heartbeat }, ?MEMBER_CHECK_STATE_EVERY }.

unreachable(timeout, S=#state{ drop_unreachable_after=DropUnrechableAfter }) when DropUnrechableAfter =:= 0 ->
  { next_state, unreachable, S, ?MEMBER_CHECK_STATE_EVERY };

unreachable(timeout, S=#state{ drop_unreachable_after=DropUnrechableAfter, last_heard=LastHeardOf, name=MemberName, membership = MembershipPid })
  when DropUnrechableAfter > 0 ->
  Timestamp = gossiperl_common:get_timestamp(),
  if
    ( Timestamp - LastHeardOf >= DropUnrechableAfter ) ->
      gen_server:cast( MembershipPid, { member_state_change, <<"member_drop">>, MemberName } ),
      { next_state, dropped, S, 0 };
    true ->
      { next_state, unreachable, S, ?MEMBER_CHECK_STATE_EVERY }
  end;

unreachable({reachable, Heartbeat}, S=#state{ name=MemberName, membership=MembershipPid }) ->
  gen_server:cast( MembershipPid, { member_state_change, <<"member_rejoin">>, MemberName } ),
  { next_state, reachable, S#state{ last_heard=Heartbeat }, ?MEMBER_CHECK_STATE_EVERY }.

dropped(_, #state{ name=MemberName, ip=MemberIp }) ->
  exit({dropped, MemberName, MemberIp}).
