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

-export([ start_link/0,
          stop/0,
          init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          code_change/3,
          terminate/2 ]).

%%%=========================================================================
%%%  API
%%%=========================================================================

-type proc_state() :: term().
-type overlay_name() :: binary().
-type summary_window() :: binary().
-type digest_type() :: binary().
-type record_data() :: { data, digest_type(), in }
                       | { data, digest_type(), out }
                       | { message_failed, decrypt, in }
                       | { message_failed, decode, in }.

-export_type([ proc_state/0,
               overlay_name/0,
               summary_window/0,
               digest_type/0,
               record_data/0 ]).

-callback supported_windows() -> [ binary() ].
-callback configure( Args :: list() )
          -> { ok, State :: proc_state() }
          |  { error, Reason :: term() }.
-callback ensure_storage( OverlayName :: overlay_name(), State :: proc_state() ) -> proc_state().
-callback record( OverlayName :: overlay_name(), Data :: record_data(), Value :: number(), State :: proc_state() ) -> proc_state().
-callback summary( OverlayName :: overlay_name(), RequestedWindow :: summary_window(), State :: proc_state() )
          -> { ok, proc_state(), term() }
          |  { error, term() }.

%%%=========================================================================
%%%  GenServer API
%%%=========================================================================

-record(state, { enabled = false :: boolean(),
                 proc_state :: proc_state(),
                 module :: pid() }).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [ application:get_env( gossiperl, statictics_backend ) ], []).

stop() -> gen_server:cast(?MODULE, stop).

init([ undefined ]) ->
  gossiperl_log:info("Statistics backend configuration not given."),
  { ok, #state{} };

init([ { ok, BackendConfiguration } ]) ->
  BackendEnabled = proplists:get_value( <<"enabled">>, BackendConfiguration, false ),
  case BackendEnabled of
    true ->
      gossiperl_log:info("Statistics backend enabled."),
      BackendEngine = proplists:get_value( <<"engine">>, BackendConfiguration, undefined ),
      case BackendEngine of
        undefined ->
          gossiperl_log:info("Statistics backend engine not configured. Please check statistics_backend/engine property."),
          { ok, #state{} };
        _ ->
          BackendModuleName = list_to_atom( binary_to_list( BackendEngine ) ),
          gossiperl_log:info("Statistics using ~p backend.", [ BackendEngine ]),
          case code:load_file( BackendModuleName ) of
            { module, Module } ->
              case BackendModuleName:configure( proplists:get_value( BackendEngine, BackendConfiguration, [] ) ) of
                { ok, State } ->
                  gossiperl_log:info("Statistics backend module loaded and started."),
                  { ok, #state{ enabled = true, module = Module, proc_state = State } };
                { error, Reason } ->
                  gossiperl_log:error("Statistics backend could not be started. Reason: ~p.", [ Reason ]),
                  { ok, #state{} }
              end;
            { error, Reason } ->
              gossiperl_log:error("Statistics disabled. Could not load intended module ~p. Reason: ~p.", [ BackendModuleName, Reason ]),
              { ok, #state{} }
          end
      end;
    false ->
      gossiperl_log:info("Statistics backend disabled."),
      { ok, #state{} }
  end.

handle_cast({ ensure_storage, _OverlayName }, S = #state{ enabled = false }) ->
  {noreply, S};

handle_cast({ ensure_storage, OverlayName }, S = #state{ enabled = true, module = Module, proc_state = ProcessState })
  when is_binary(OverlayName) ->
  { noreply, S#state{ proc_state = Module:ensure_storage( OverlayName, ProcessState ) } };

handle_cast({ record, _OverlayName, _Key, _Value }, S = #state{ enabled = false }) ->
  { noreply, S };

handle_cast({ record, OverlayName, { data, DigestType, in }, Value }, S = #state{ enabled = true, module = Module, proc_state = ProcessState })
  when is_binary(OverlayName) andalso is_atom(DigestType) ->
  BinDigestType = list_to_binary(atom_to_list( DigestType )),
  { noreply, S#state{ proc_state = Module:record( OverlayName, { data, BinDigestType, in }, Value, ProcessState ) } };

handle_cast({ record, OverlayName, { data, DigestType, in }, Value }, S = #state{ enabled = true, module = Module, proc_state = ProcessState })
  when is_binary(DigestType) ->
  { noreply, S#state{ proc_state = Module:record( OverlayName, { data, DigestType, in }, Value, ProcessState ) } };

handle_cast({ record, OverlayName, { data, DigestType, out }, Value }, S = #state{ enabled = true, module = Module, proc_state = ProcessState })
  when is_binary(OverlayName) andalso is_atom(DigestType) ->
  BinDigestType = list_to_binary(atom_to_list( DigestType )),
  { noreply, S#state{ proc_state = Module:record( OverlayName, { data, BinDigestType, out }, Value, ProcessState ) } };

handle_cast({ record, OverlayName, { message_failed, decrypt, in }, Value }, S = #state{ enabled = true, module = Module, proc_state = ProcessState })
  when is_binary(OverlayName) andalso is_number(Value) ->
  { noreply, S#state{ proc_state = Module:record( OverlayName, { message_failed, decrypt, in }, Value, ProcessState ) } };

handle_cast({ record, OverlayName, { message_failed, decode, in }, Value }, S = #state{ enabled = true, module = Module, proc_state = ProcessState })
  when is_binary(OverlayName) andalso is_number(Value) ->
  { noreply, S#state{ proc_state = Module:record( OverlayName, { message_failed, decode, in }, Value, ProcessState ) } };

handle_cast(stop, LoopData) ->
  {noreply, LoopData}.

handle_info(_, LoopData) ->
  {noreply, LoopData}.

handle_call(supported_windows, From, S = #state{ enabled = false }) ->
  gen_server:reply( From, [] ),
  { noreply, S };

handle_call(supported_windows, From, S = #state{ enabled = true, module = Module }) ->
  gen_server:reply( From, Module:supported_windows() ),
  { noreply, S };

handle_call({ summary, _OverlayName, _RequestedWindow }, From, S = #state{ enabled = false }) ->
  gen_server:reply( From, not_running ),
  { noreply, S };

handle_call({ summary, OverlayName, RequestedWindow }, From, S = #state{ enabled = true, module = Module, proc_state = ProcessState })
  when is_binary( OverlayName ) andalso is_binary( RequestedWindow ) ->
  case Module:summary( OverlayName, RequestedWindow, ProcessState ) of
    { ok, NewProcessState, Result } ->
      gen_server:reply( From, Result ),
      { noreply, S#state{ proc_state = NewProcessState } };
    { error, ErrorReason } ->
      gen_server:reply( From, ErrorReason ),
      { noreply, S }
  end;

handle_call({message, Msg}, From, LoopData) ->
  gossiperl_log:warn("Statistics unhandled handle_call: from ~p, message is: ~p", [From, Msg]),
  {reply, ok, LoopData}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, LoopData) ->
  {ok, LoopData}.
  