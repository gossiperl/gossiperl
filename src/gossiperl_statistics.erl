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

-module(gossiperl_statistics).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("gossiperl.hrl").

-include_lib("erflux/include/erflux.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

init([]) ->
  
  case application:get_env( gossiperl, statictics_backend ) of
    undefined ->
      
      gossiperl_log:info("Statistics not enabled. Backend configuration not given."),
      { ok, { statistics, not_running } };

    { ok, BackendConfiguration } ->
      case BackendConfiguration of
        
        [ { <<"enabled">>, true }, { <<"influxdb">>, BackendConfigurationSettings } ] ->

          case influx_db_config( BackendConfigurationSettings, #erflux_config{} ) of
            { ok, BackendConfigRecord } ->

              application:start( ssl ),
              application:start( idna ),
              application:start( hackney ),
              application:start( erflux ),

              { ok, ErfluxPid } = erflux_http:start_link( gossiperl_erflux, BackendConfigRecord ),
              gossiperl_log:notice("Using InfluxDB statistics backend with configuration ~p.", [ BackendConfigRecord ]),
              { ok, { statistics, influxdb, ErfluxPid } };
            { error, { badarg, BadArg } } ->
              gossiperl_log:warn("Unsupported InfluxDB configuration parameter: ~p.", [BadArg]),
              { ok, { statistics, not_running } }
          end;

        _ ->
          
          gossiperl_log:info("Statistics not enabled. Unsupported backend configuration."),
          { ok, { statistics, not_running } }

      end
  end.

handle_cast({ ensure_storage, _OverlayName }, { statistics, not_running }) ->
  {noreply, { statistics, not_running }};

handle_cast({ ensure_storage, OverlayName }, { statistics, influxdb, ErfluxPid })
  when is_binary(OverlayName) ->
  case erflux_http:create_database( ErfluxPid, OverlayName ) of
    ok ->
      gossiperl_log:info("Created storage for statistics of ~p.", [ OverlayName ]);
    { error, 409 } ->
      gossiperl_log:warn("Statistics storage for ~p already exists.", [ OverlayName ]);
    { error, Reason } ->
      gossiperl_log:warn("Problem creating storage for statistics of ~p, reason ~p.", [ OverlayName, Reason ])
  end,
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast({ record, _OverlayName, _Key, _Value }, { statistics, not_running }) ->
  {noreply, { statistics, not_running }};

handle_cast({ record, OverlayName, { data, DigestType, in }, Value }, { statistics, influxdb, ErfluxPid })
  when is_binary(OverlayName) andalso is_atom(DigestType) ->
  BinDigestType = list_to_binary(atom_to_list( DigestType )),
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<BinDigestType/binary, "-in">>,
                                                   [ { count, 1 },
                                                     { size, Value } ] ),
  gen_server:cast(self(), { analyse_write_result, OverlayName, StatisticsWriteResult, { data, DigestType, in } }),
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast({ record, OverlayName, { data, DigestType, in }, Value }, { statistics, influxdb, ErfluxPid })
  when is_binary(DigestType) ->
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<DigestType/binary, "-in">>,
                                                   [ { count, 1 },
                                                     { size, Value } ] ),
  gen_server:cast(self(), { analyse_write_result, OverlayName, StatisticsWriteResult, { data, DigestType, in } }),
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast({ record, OverlayName, { data, DigestType, out }, Value }, { statistics, influxdb, ErfluxPid })
  when is_binary(OverlayName) andalso is_atom(DigestType) ->
  BinDigestType = list_to_binary(atom_to_list( DigestType )),
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<BinDigestType/binary, "-out">>,
                                                   [ { count, 1 },
                                                     { size, Value } ] ),
  gen_server:cast(self(), { analyse_write_result, OverlayName, StatisticsWriteResult, { data, DigestType, out } }),
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast({ record, OverlayName, { message_failed, decrypt, in }, Value }, { statistics, influxdb, ErfluxPid })
  when is_binary(OverlayName) andalso is_number(Value) ->
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<"message-failed-decrypt-in">>,
                                                   [ { count, Value } ] ),
  gen_server:cast(self(), { analyse_write_result, OverlayName, StatisticsWriteResult }),
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast({ record, OverlayName, { message_failed, decode, in }, Value }, { statistics, influxdb, ErfluxPid })
  when is_binary(OverlayName) andalso is_number(Value) ->
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<"message-failed-decode-in">>,
                                                   [ { count, Value } ] ),
  gen_server:cast(self(), { analyse_write_result, OverlayName, StatisticsWriteResult, { message_failed, decode, in } }),
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast({ analyse_write_result, _OverlayName, ok, _Key }, { statistics, influxdb, ErfluxPid }) ->
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast({ analyse_write_result, OverlayName, { error, Reason }, Key }, { statistics, influxdb, ErfluxPid }) ->
  gossiperl_log:error("Statistics for ~p, ~p not recorded. Reason ~p.", [ OverlayName, Key, Reason ]),
  {noreply, { statistics, influxdb, ErfluxPid }};

handle_cast(stop, LoopData) ->
  {noreply, LoopData}.

handle_info(Msg, LoopData) ->
  case LoopData of
    {statistics, Config, _} ->
      gossiperl_log:warn("[~p] handle_info: ~p", [Config#overlayConfig.name, Msg]);
    _ ->
      gossiperl_log:warn("[~p] handle_info: ~p", [self(), Msg])
  end,
  {noreply, LoopData}.

handle_call({ get_summary, _OverlayName, _RequestedWindow }, From, { statistics, not_running }) ->
  gen_server:reply( From, not_running ),
  { noreply, { statistics, not_running } };

handle_call({ get_summary, OverlayName, RequestedWindow }, From, { statistics, influxdb, ErfluxPid })
  when is_binary( OverlayName ) andalso is_binary( RequestedWindow ) ->

  % get all time series:
  QueryResult = erflux_http:q(ErfluxPid, OverlayName, <<"select * from /.*/ limit 1">>),
  SummaryResult = lists:foldl( fun( [{<<"name">>, SeriesName}, {<<"columns">>, SeriesColumns}, _Points], Acc ) ->

    WhereCond = influx_db_where( RequestedWindow ),
    SeriesQueryResult = case lists:member(<<"size">>, SeriesColumns) of
      true ->
        erflux_http:q(ErfluxPid, OverlayName, <<"select sum(count) as sum_packets, sum(size) as total_size from \"", SeriesName/binary, "\"", WhereCond/binary>>);
      false ->
        erflux_http:q(ErfluxPid, OverlayName, <<"select sum(count) as sum_packets from \"", SeriesName/binary, "\"", WhereCond/binary>>)
    end,

    case SeriesQueryResult of
      [] ->
        Acc ++ [ { SeriesName, nil } ];
      [ [ { <<"name">>, SeriesName }, { <<"columns">>, ReturnedColumns }, { <<"points">>, [ ReturnedPoints ] } ] ] ->
        Zipped = lists:zip( ReturnedColumns, ReturnedPoints ),
        Acc ++ [ { SeriesName, lists:keydelete(<<"time">>, 1, Zipped) } ]
    end

  end, [], QueryResult),
  
  gen_server:reply( From, SummaryResult ),

  { noreply, { statistics, influxdb, ErfluxPid } };

handle_call({message, Msg}, From, LoopData) ->
  gossiperl_log:warn("handle_call: from ~p, message is: ~p", [From, Msg]),
  {reply, ok, LoopData}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, LoopData) ->
  {ok, LoopData}.

%% private

%% @doc Get erflux configuration from given data.
-spec influx_db_config( [ { binary(), term() } ], erflux:erflux_config() ) -> { ok, erflux:erflux_config() } | { error, { bagarg, term() } }.
influx_db_config([ H | T ], ConfigRecord) ->
  case H of
    { <<"host">>, InfluxDBHost } ->
      influx_db_config( T, ConfigRecord#erflux_config{ host = InfluxDBHost } );
    { <<"port">>, InfluxDBPort } ->
      influx_db_config( T, ConfigRecord#erflux_config{ port = InfluxDBPort } );
    { <<"username">>, InfluxDBUsername } ->
      influx_db_config( T, ConfigRecord#erflux_config{ username = InfluxDBUsername } );
    { <<"password">>, InfluxDBPassword } ->
      influx_db_config( T, ConfigRecord#erflux_config{ password = InfluxDBPassword } );
    { <<"timeout">>, InfluxDBTimeout } ->
      influx_db_config( T, ConfigRecord#erflux_config{ timeout = InfluxDBTimeout } );
    { <<"protocol">>, InfluxDBProtocol } ->
      influx_db_config( T, ConfigRecord#erflux_config{ protocol = InfluxDBProtocol } );
    AnyOther ->
      { error, { badarg, AnyOther } }
  end;

influx_db_config([], ConfigRecord) ->
  { ok, ConfigRecord }.

%% @doc Get InfluxDB where condition based on the requested statistic window.
-spec influx_db_where( binary() ) -> binary().
influx_db_where( <<"total">> ) ->
  <<>>;
influx_db_where( RequestedWindow ) when is_binary(RequestedWindow) ->
  <<" where time > now() - ", RequestedWindow/binary>>.

