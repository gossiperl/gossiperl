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

-module(gossiperl_statistics_influxdb).

-behaviour(gossiperl_statistics).

-export([ supported_windows/0,
          configure/1,
          ensure_storage/2,
          record/4,
          summary/3 ]).

-type write_result() :: ok | { error, term() }.

-include_lib("erflux/include/erflux.hrl").

-record(state, { proc :: pid(),
                 configuration :: term() }).

%%%=========================================================================
%%%  GossiperlStatistics API
%%%=========================================================================

%% @doc Get the list of supported windows.
-spec supported_windows() -> [ binary() ].
supported_windows() ->
  [ <<"total">>, <<"1d">>, <<"12h">>, <<"6h">>, <<"3h">>, <<"1h">>, <<"30m">>, <<"10m">>, <<"1m">> ].

%% @doc Configure connector.
-spec configure( list() ) -> { ok, gossiperl_statistics:proc_state() } | { error, term() }.
configure( Args ) ->
  case influx_db_config( Args, #erflux_config{} ) of
    { ok, BackendConfigRecord } ->
      application:start( ssl ),
      application:start( idna ),
      application:start( hackney ),
      application:start( erflux ),
      { ok, ErfluxPid } = erflux_http:start_link( gossiperl_erflux, BackendConfigRecord ),
      gossiperl_log:notice("[INFLUXDB] Using InfluxDB statistics backend with configuration ~p.", [ BackendConfigRecord ]),
      { ok, #state{ proc = ErfluxPid, configuration = Args } };
    { error, { badarg, BadArg } } ->
      gossiperl_log:warn("[INFLUXDB] Unsupported InfluxDB configuration parameter: ~p.", [BadArg]),
      { error, { badarg, BadArg } }
  end.

%% @doc Ensure storage for overlay statistics.
-spec ensure_storage( gossiperl_statistics:overlay_name(), gossiperl_statistics:proc_state() ) -> gossiperl_statistics:proc_state().
ensure_storage( OverlayName, S = #state{ proc = ErfluxPid } ) ->
  case erflux_http:create_database( ErfluxPid, OverlayName ) of
    ok ->
      gossiperl_log:info("[INFLUXDB] Created storage for statistics of ~p.", [ OverlayName ]);
    { error, 409 } ->
      gossiperl_log:warn("[INFLUXDB] Statistics storage for ~p already exists.", [ OverlayName ]);
    { error, Reason } ->
      gossiperl_log:warn("[INFLUXDB] Problem creating storage for statistics of ~p, reason ~p.", [ OverlayName, Reason ])
  end,
  S.

%% @doc Record statistic.
-spec record( gossiperl_statistics:overlay_name(), gossiperl_statistics:record_data(), number(), gossiperl_statistics:proc_state() ) -> gossiperl_statistics:proc_state().
record( OverlayName, { data, DigestType, in }, Value, S = #state{ proc = ErfluxPid } ) ->
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<DigestType/binary, "-in">>,
                                                   [ { count, 1 },
                                                     { size, Value } ] ),
  _ = analyse_write_result( OverlayName, StatisticsWriteResult, { data, DigestType, in } ),
  S;

record( OverlayName, { data, DigestType, out }, Value, S = #state{ proc = ErfluxPid } ) ->
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<DigestType/binary, "-in">>,
                                                   [ { count, 1 },
                                                     { size, Value } ] ),
  _ = analyse_write_result( OverlayName, StatisticsWriteResult, { data, DigestType, out } ),
  S;

record( OverlayName, { message_failed, decrypt, in }, Value, S = #state{ proc = ErfluxPid } ) ->
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<"message-failed-decode-in">>,
                                                   [ { count, Value } ] ),
  _ = analyse_write_result( OverlayName, StatisticsWriteResult, { message_failed, decrypt, in } ),
  S;

record( OverlayName, { message_failed, decode, in }, Value, S = #state{ proc = ErfluxPid } ) ->
  StatisticsWriteResult = erflux_http:write_point( ErfluxPid,
                                                   OverlayName,
                                                   <<"message-failed-decode-in">>,
                                                   [ { count, Value } ] ),
  _ = analyse_write_result( OverlayName, StatisticsWriteResult, { message_failed, decode, in } ),
  S.

%% doc Get statistics summary for a given window.
-spec summary( gossiperl_statistics:overlay_name(), binary(), gossiperl_statistics:proc_state() )
      -> { ok, gossiperl_statistics:proc_state(), term() }
      |  { error, term() }.
summary( OverlayName, RequestedWindow, S = #state{ proc = ErfluxPid } ) ->
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
  { ok, S, SummaryResult }.

%%%=========================================================================
%%%  Private API
%%%=========================================================================

%% @doc Report statistic write error.
-spec analyse_write_result( gossiperl_statistics:overlay_name(), write_result(), gossiperl_statistics:record_data() ) -> ok | { error, term() }.
analyse_write_result( _, ok, _ ) -> ok;
analyse_write_result( OverlayName, { error, Reason }, Key ) ->
  gossiperl_log:error("[INFLUXDB] Statistics for ~p, ~p not recorded. Reason ~p.", [ OverlayName, Key, Reason ]),
  { error, Reason }.

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

