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

-module(web_api_v1_digest).

-export([init/2]).

-include("gossiperl.hrl").

init(Req, Opts) ->
  {ok, reply(cowboy_req:method(Req), Req), Opts}.

reply(<<"POST">>, Req) ->
  {ok, Data, _} = cowboy_req:body(Req, []),
  RequestedOverlayName = cowboy_req:binding(overlay, Req),
  case gossiperl_configuration:for_overlay( RequestedOverlayName ) of
    { ok, { _, OverlayConfig } } ->
      case gen_server:call( gossiperl_web, { authorize_token, OverlayConfig, Req } ) of
        { ok, token_ok } ->
          try
            case jsx:decode( Data ) of
              [ { DigestType, DigestData }, { <<"recipient_ip">>, RecipientIp }, { <<"recipient_port">>, RecipientPort } ] ->
                Member = #digestMember{ member_ip = RecipientIp, member_port = RecipientPort },
                % TODO: this isn't right, we should avoid changin to atom:
                deliver_digest( list_to_atom(binary_to_list(DigestType)), DigestData, OverlayConfig, Member ),
                cowboy_req:reply(200, [
                  {<<"content-type">>, <<"application/json; charset=utf-8">>}
                ], jsx:encode( [{ status, <<"ok">> }] ), Req);
              [ { DigestType, DigestData } ] ->
                % TODO: this isn't right, we should avoid changin to atom:
                deliver_digest( list_to_atom(binary_to_list(DigestType)), DigestData, OverlayConfig ),
                cowboy_req:reply(200, [
                  {<<"content-type">>, <<"application/json; charset=utf-8">>}
                ], jsx:encode( [{ status, <<"ok">> }] ), Req);
              _ ->
                cowboy_req:reply(409, [], <<"Given JSON arguments not complete. Required fields: digest_type. Optional: specification.">>, Req)
            end
          catch
            Error:Reason ->
              error_logger:info_msg("~p, ~p", [Error, Reason]),
              cowboy_req:reply(400, [], <<"Error while parsing JSON data.">>, Req)
          end;
        { error, token_mismatch } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Send digest from ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req);
        { error, no_auth } ->
          cowboy_req:reply(401, [
            {<<"www-authenticate">>, <<"Basic realm=\"Send digest from ", RequestedOverlayName/binary ,"\"">>}
          ], <<"Authorization required.">>, Req)
      end;
    { error, no_config } ->
      cowboy_req:reply(404, [], "Overlay not found.", Req)
  end;
  
reply(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

deliver_digest(DigestType, DigestData, OverlayConfig, Member) when is_atom(DigestType) ->
  { StructInfo, RecordTuple } = get_digest_record_from_string(DigestType, DigestData),
  { ok, SerializedDigest, _ } = gen_server:call( gossiperl_serialization, { serialize,
                                                                            DigestType,
                                                                            eval_string( RecordTuple ),
                                                                            StructInfo } ),
  ?MESSAGING( OverlayConfig ) ! { send_digest, Member, digestEnvelope, SerializedDigest }.

deliver_digest(DigestType, DigestData, OverlayConfig) when is_atom(DigestType) ->
  { StructInfo, RecordTuple }         = get_digest_record_from_string(DigestType, DigestData),
  { ok, SerializedDigest, MessageId } = gen_server:call( gossiperl_serialization, { serialize,
                                                                                    DigestType,
                                                                                    eval_string( RecordTuple ),
                                                                                    StructInfo } ),
  ?SUBSCRIPTIONS(OverlayConfig) ! { notify,
                                    list_to_binary( atom_to_list( DigestType ) ),
                                    SerializedDigest,
                                    { 0,0,0,0 },
                                    MessageId }.

get_digest_record_from_string( DigestType, DigestData ) ->
  { BinaryRecord, StructInfo } = json_to_erlang_data( DigestData ),
  RecordDef = iolist_to_binary(io_lib:format("{~p", [ DigestType ])),
  { StructInfo, binary_to_list(<<RecordDef/binary, BinaryRecord/binary, "}.">>) }.

json_to_erlang_data( DigestData ) ->
  lists:foldl(fun([ { _Name, Value }, { DataType, Order } ], Acc) ->
    { BinaryData, { struct, StructInfo } } = Acc,
    FormattedValue = iolist_to_binary( io_lib:format(",~p", [ Value ] ) ),
    NewBinary = <<BinaryData/binary, FormattedValue/binary>>,
    NewStructInfo = ( StructInfo ++ [ { Order, list_to_atom(binary_to_list(DataType)) } ] ),
    { NewBinary, { struct, NewStructInfo } }
  end, { <<"">>, { struct, [] } }, DigestData).

eval_string(S) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Value, _NewBindings} = erl_eval:exprs(Parsed,[]),
    Value.
