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

-module(gossiperl_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).
-export([configure_from_file/0]).

-include("gossiperl.hrl").

%% @doc Starts gossiperl application.
start(_Type, _Args) ->
  configure_from_file(),
  gossiperl_sup:start_link().

%% @doc Prepares gossiperl for stop.
prep_stop(_State) ->
  gossiperl_log:info( "Stopping application gossiperl..." ),
  gossiperl_sup:stop_overlays().

%% @doc Stops gossiperl application.
stop(_State) ->
  ok.

%% private

%% @doc Configures gossiperl from configuration file, if file exists.
-spec configure_from_file() -> ok.
configure_from_file() ->
  case filelib:is_regular( ?ETC_CONFIG_PATH ) of
    false ->
      PrivDir = gossiperl_common:privdir(),
      case filelib:is_regular( filename:join(PrivDir, binary_to_list(?PRIV_CONFIG_PATH)) ) of
        false ->
          no_file;
        true ->
          configure_with_existing_file( filename:join(PrivDir, binary_to_list(?PRIV_CONFIG_PATH)) )
      end;
    true ->
      configure_with_existing_file(?ETC_CONFIG_PATH)
  end.

configure_with_existing_file( Filepath ) ->
  {ok, Data} = file:read_file( Filepath ),
  JsonData = gossiperl_common:binary_join( binary:split(Data, [<<"\n">>], [global]), <<"">> ),
  try
    ParsedConfig = jsx:decode( JsonData ),
    gossiperl_log:debug("Configuring from file:"),
    [ configure_property( list_to_atom(binary_to_list( Key )), Value ) || { Key, Value } <- ParsedConfig ],
    ok
  catch
    _Error:Reason -> { error, Reason }
  end.

%% @doc Configures single environment property.
-spec configure_property(atom(), any()) -> ok.
configure_property( Key, Value ) when is_atom(Key) ->
  gossiperl_log:debug(" -> ~p : ~p", [ Key, Value ]),
  application:set_env( gossiperl, Key, Value ).