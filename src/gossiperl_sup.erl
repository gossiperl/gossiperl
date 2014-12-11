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

-module(gossiperl_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, stop_overlays/0]).
-export([
  add_overlay/2,
  reconfigure_overlay/2,
  remove_overlay/2,
  list_overlays/0,
  list_overlays/1]).

-include("gossiperl.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  gossiperl_common:seed_random(),
  ets:new(gossiperl_overlay_configuration, [set, named_table, public]),
  {ok, {{one_for_all, 10, 10}, [
    ?GOSSIPER_MEMBER( gossiperl_web ),
    ?GOSSIPER_MEMBER( gossiperl_serialization ),
    ?GOSSIPER_MEMBER( gossiperl_statistics ) ]}}.

%% @doc Add an overlay and start it.
-spec add_overlay( atom(), gossiperl_config() ) -> supervisor:startchild_ret() | supervisor:startchild_err() | { error, { atom(), any() } }.
add_overlay(OverlayName, Config = #overlayConfig{}) when is_atom(OverlayName) ->
  case gossiperl_configuration:validate( Config ) of
    { ok, Config } ->
      supervisor:start_child(?MODULE, ?GOSSIPER_OVERLAY( OverlayName, Config ));
    { error, Reason } ->
      { error, Reason }
  end.

%% @doc Reconfigure running overlay with new configuration record.
-spec reconfigure_overlay( gossiperl_config(), gossiperl_config() ) -> { ok, gossiperl_config(), gossiperl_config() } | { error, atom() }.
reconfigure_overlay( OldConfig = #overlayConfig{}, NewConfig = #overlayConfig{} ) ->
  case gossiperl_configuration:validate( NewConfig ) of
    { ok, NewConfig } ->
      PreparedConfig = gossiperl_configuration:store_config( NewConfig#overlayConfig{ internal=OldConfig#overlayConfig.internal } ),
      Pids = [ ?ENCRYPTION( PreparedConfig ), ?MEMBERSHIP( PreparedConfig ), ?MESSAGING( PreparedConfig ), ?SUBSCRIPTIONS( PreparedConfig ) ],
      [ Pid ! { update_config, PreparedConfig } || Pid <- Pids ],
      { ok, OldConfig, PreparedConfig };
    { error, Reason } ->
      { error, Reason }
  end.

%% @doc Stop and remove overlay.
-spec remove_overlay( atom(), gossiperl_config() ) -> true | {error, overlay_removal_error()} | {error, overlay_termination_error()}.
remove_overlay(OverlayName, Config = #overlayConfig{}) when is_atom(OverlayName) ->
  _ = gen_server:call( ?MESSAGING( Config ), stop ),
  case supervisor:terminate_child(?MODULE, OverlayName) of
    ok ->
      case supervisor:delete_child(?MODULE, OverlayName) of
        ok ->
          gossiperl_configuration:remove_configuration_for(OverlayName);
        { error, Reason } ->
          { error, Reason }
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc List names of running overlays.
-spec list_overlays() -> [ atom() ].
list_overlays() ->
  [ OverlayName || { OverlayName, _OverlayData } <- gossiperl_configuration:list_overlays() ].

%% @doc List names of running overlays.
-spec list_overlays(with_tokens) -> [ [ { binary(), binary() } ] ].
list_overlays(with_tokens) ->
  [ [ { OverlayName, OverlayData#overlayConfig.internal#internalConfig.webToken } ] || { OverlayName, OverlayData } <- gossiperl_configuration:list_overlays() ].

%% @doc Called by application:prep_stop.
-spec stop_overlays() -> ok.
stop_overlays() ->
  [ begin
      OverlayNameAtom = list_to_atom( binary_to_list( OverlayName ) ),
      gossiperl_log:info("Removing overlay ~p...", [ OverlayNameAtom ]),
      remove_overlay( OverlayNameAtom, OverlayData ),
      gossiperl_log:info("Overlay ~p removed.", [ OverlayNameAtom ])
    end || { OverlayName, OverlayData } <- gossiperl_configuration:list_overlays() ],
  ok.  
