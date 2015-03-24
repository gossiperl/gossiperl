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

-module(gossiperl_overlay_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-include("gossiperl.hrl").

%% @doc Starts an overlay supervisor.
-spec start_link( gossiperl_configuration:gossiperl_config() ) -> supervisor:startlink_ret() | supervisor:startlink_err() | { error, { atom(), any() } }.
start_link(Config) ->
  case gossiperl_configuration:setup(Config) of
    { error, Reason } ->
      { error, Reason };
    Config2 ->
      supervisor:start_link({local, list_to_atom("overlay_" ++ Config2#overlayConfig.internal#internalConfig.nameList)}, ?MODULE, [Config2])
  end.

%% @doc initialises an overlay supervisor. Called by start_link.
-spec init([ gossiperl_configuration:gossiperl_config() ]) -> {ok,{{supervisor:strategy(),non_neg_integer(),non_neg_integer()},[supervisor:child_spec()]}} | ignore.
init([Config]) ->
  gossiperl_log:notice("GossiperConfiguration is :: ~p", [Config]),
  {ok, {{one_for_one, 10, 10},
    [ ?OVERLAY_MEMBER( ?SUBSCRIPTIONS( Config ), gossiperl_subscriptions, Config ),
      ?OVERLAY_MEMBER( ?MEMBERSHIP( Config ), gossiperl_membership, Config ),
      ?OVERLAY_MEMBER( ?ENCRYPTION( Config ), gossiperl_encryption, Config ),
      ?OVERLAY_MEMBER( ?MESSAGING( Config ), gossiperl_messaging, Config ) ]}}.
