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

-module(gossiperl_common).

-export([get_timestamp/0, get_timestamp/1]).
-export([
  get_iface_ip/1,
  get_all_ipv4_addrs/0,
  parse_binary_ip/1,
  seed_random/0,
  binary_join/2,
  locate_file/1,
  ip_to_binary/1,
  privdir/0]).

-include("gossiperl.hrl").

%% @doc Get UNIX timestamp.
-spec get_timestamp() -> integer().
get_timestamp() ->
  {Mega,Sec,Micro} = os:timestamp(),
  trunc( ((Mega*1000000+Sec)*1000000+Micro) / 1000000 ).

%% @doc Get UNIX timestamp from OS timestamp.
-spec get_timestamp( timestamp() ) -> integer().
get_timestamp({Mega,Sec,Micro}) ->
  trunc( ((Mega*1000000+Sec)*1000000+Micro) / 1000000 ).

%% @doc Get first IPv4 address of the interface, if any.
-spec get_iface_ip( binary() ) -> { ok, inet:ip_address() } | { error, no_addr } | { error, no_iface }.
get_iface_ip(Lookup) when is_binary( Lookup ) ->
  case inet:getifaddrs() of
    { ok, Interfaces } ->
      case lists:filter(fun({ IfaceName, _ }) -> list_to_binary( IfaceName ) =:= Lookup end, Interfaces) of
        [ { _, Options } ] ->
          Addresses = lists:filter( fun( { OptionName, Value } ) ->
                        case { OptionName, Value } of { addr, {_,_,_,_} } -> true; _ -> false end
                      end, Options ),
          case Addresses of
            [] -> { error, no_addr }; [ { addr, Ip } | _ ] -> { ok, Ip }
          end;
        [] -> { error, no_iface }
      end;
    _ -> { error, no_iface }
  end.

%% @doc Returns a of IPv4 addresses of every interface installed on the host machine.
-spec get_all_ipv4_addrs() -> { ok, [ inet:ip_address() ] } | { error, no_iface }.
get_all_ipv4_addrs() ->
  case inet:getifaddrs() of
    { ok, Interfaces } ->
      [ Ip || { addr, Ip } <- lists:flatten(
        lists:map(fun({_, Options}) ->
          lists:filter( fun({ OptName, Value }) ->
            case { OptName, Value } of { addr, {_,_,_,_} } -> true; _ -> false end
          end, Options )
        end, Interfaces)
      )];
    _ -> { error, no_iface }
  end.

-spec parse_binary_ip( binary() ) -> inet:ip_address() | { error, { no_ip, any() } }.
parse_binary_ip(BinaryIp) when is_binary( BinaryIp ) ->
  case inet:parse_address( binary_to_list( BinaryIp ) ) of
    { ok, Ip } -> Ip;
    _          -> { error, { not_ip, BinaryIp } }
  end.

-spec seed_random() -> ok.
seed_random() ->
    % to provide better seeding than erlang:now() or os:timestamp()
    <<B1:32/unsigned-integer,
      B2:32/unsigned-integer,
      B3:32/unsigned-integer,
      _B4:32/unsigned-integer>> = try crypto:strong_rand_bytes(16)
    catch
        error:low_entropy ->
            gossiperl_log:err("seed(): low_entropy!"),
            crypto:rand_bytes(16)
    end,
    random:seed(B1, B2, B3),
    ok.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).

-spec ip_to_binary( inet:ip_address() ) -> binary().
ip_to_binary(IPAddress) ->
  list_to_binary( inet:ntoa( IPAddress ) ).

locate_file(Filename) ->
  case string:substr(Filename, 1, 1) of
    "/" ->
      Filename;
    _ ->
      filename:join( privdir(), Filename )
  end.

privdir() ->
  case code:priv_dir(gossiperl) of
    {error, _} ->
      EbinDir = filename:dirname(code:which(?MODULE)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Dir -> Dir
  end.
