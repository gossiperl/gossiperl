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

-module(gossiperl_encryption).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("gossiperl.hrl").

start_link(Config) ->
  gen_server:start_link({local, ?ENCRYPTION(Config)}, ?MODULE, [Config], []).

init([Config]) ->
  {ok, { encryption, Config#overlayConfig{ symmetric_key = erlsha2:sha256( Config#overlayConfig.symmetric_key ) } }}.

stop() -> gen_server:cast(?MODULE, stop).

terminate(_Reason, _LoopData) ->
  {ok}.

handle_cast(stop, LoopData) ->
  {noreply, LoopData}.

handle_info({ update_config, NewConfig = #overlayConfig{} }, {encryption, _Config}) ->
  gossiperl_log:notice("[~p] Reconfiguring encryption component with ~p.", [ NewConfig#overlayConfig.name, NewConfig ]),
  {noreply, {encryption, NewConfig#overlayConfig{ symmetric_key = erlsha2:sha256( NewConfig#overlayConfig.symmetric_key ) }}};

%% @doc Encrypt Msg and deliver to a caller.
handle_info({ encrypt, MsgType, Msg, CallerPid, ReceivingMember = #digestMember{} }, { encryption, Config })
  when is_atom(MsgType) andalso is_pid(CallerPid) ->
  IV = crypto:next_iv( aes_cbc, Msg ),
  Encrypted = crypto:block_encrypt( aes_cbc256, Config#overlayConfig.symmetric_key, IV, <<IV/binary, (?AES_PAD( Msg ))/binary>> ),
  CallerPid ! { message_encrypted, { ok,
                                     MsgType,
                                     Encrypted,
                                     ReceivingMember } },
  {noreply, { encryption, Config }};

%% @doc Decrypt Msg and deliver to a caller, forward given state.
handle_info({ decrypt, <<IV:16/binary, Msg/binary>>, CallerPid, State }, { encryption, Config })
  when is_pid(CallerPid) ->
  try
    Decrypted = crypto:block_decrypt( aes_cbc256, Config#overlayConfig.symmetric_key, IV, Msg ),
    CallerPid ! { message_decrypted, { ok,
                                       Decrypted,
                                       State } }
  catch
    _Error:_Reason ->
      CallerPid ! { message_decrypted, { error, decryption_failed } }
  end,
  {noreply, { encryption, Config }};

handle_info(_Info, LoopData) ->
  {noreply, LoopData}.

handle_call(_Msg, _From, LoopData) ->
  {reply, ok, LoopData}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
