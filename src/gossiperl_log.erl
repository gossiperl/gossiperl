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

-module(gossiperl_log).

-export([debug/1, debug/2, info/1, info/2, notice/1, notice/2, warn/1, warn/2, err/1, err/2]).
-export([critical/1, critical/2, alert/1, alert/2, emergency/1, emergency/2, log_none/1, log_none/2]).

debug(Format) ->
  debug(Format, []).
debug(Format, Args) when is_list(Args) ->
  lager:log(debug, [], Format, Args).

info(Format) ->
  info(Format, []).
info(Format, Args) when is_list(Args) ->
  lager:log(info, [], Format, Args).

notice(Format) ->
  notice(Format, []).
notice(Format, Args) when is_list(Args) ->
  lager:log(notice, [], Format, Args).

warn(Format) ->
  warn(Format, []).
warn(Format, Args) when is_list(Args) ->
  lager:log(warning, [], Format, Args).

err(Format) ->
  err(Format, []).
err(Format, Args) when is_list(Args) ->
  lager:log(error, [], Format, Args).

critical(Format) ->
  critical(Format, []).
critical(Format, Args) when is_list(Args) ->
  lager:log(critical, [], Format, Args).

alert(Format) ->
  alert(Format, []).
alert(Format, Args) when is_list(Args) ->
  lager:log(alert, [], Format, Args).

emergency(Format) ->
  emergency(Format, []).
emergency(Format, Args) when is_list(Args) ->
  lager:log(emergency, [], Format, Args).

log_none(Format) ->
  log_none(Format, []).
log_none(Format, Args) when is_list(Args) ->
  lager:log(log_none, [], Format, Args).
