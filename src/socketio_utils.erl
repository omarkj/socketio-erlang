% Socket.IO server for Erlang
% 
% Portions of this file are taken from couchd_utils.erl available at
% https://github.com/apache/couchdb/blob/trunk/src/couchdb/couch_util.erl
% 
% Copyright (C) 2011, Kóði ehf, Ómar Yasin <omar@kodiak.is>
% 
% All rights reserved.
% 
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
% 
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%    following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%    the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%    products derived from this software without specific prior written permission.
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
-module (socketio_utils).

-export ([encode/1, decode/2, get_heartbeat/1, ref_to_msg/1, random/0]).

% Encode messages to Socket.IO format
encode({json, Message}) ->
  encode(binary:list_to_bin([<<"\~j\~">>, Message]));
encode(Message) ->
  Len = binary:list_to_bin(integer_to_list(erlang:size(Message))), % Yuck
  binary:list_to_bin([<<"\~m\~">>, Len, <<"\~m\~">>, Message]).

% Messages look like:
% ~m~MSGLENGTH~m~MESSAGE or
% ~m~MSGLENGTH~m~~j~JSON
% TODO: Add support for JSON messages
decode(<<"\~m\~", Message/binary>>, Buffer) ->
	case binary:split(Message, <<"\~m\~">>) of
		[_|[Message0]] ->
			case binary:match(Message0, <<"\~m\~">>) of
				{Beginning, _} ->
					<<Message1:Beginning/binary, Rest/binary>> = Message0,
					NewBuffer = case is_heartbeat(Message1) of
						false -> Buffer ++ [Message1];
						true -> Buffer
					end,
					decode(Rest, NewBuffer);
				nomatch ->
					NewBuffer = case is_heartbeat(Message0) of
						false -> Buffer ++ [Message0];
						true -> Buffer
					end,
					NewBuffer
			end
	end;
decode(_, Buffer) ->
  Buffer.

is_heartbeat(Message) ->
	case Message of
		<<"\~h\~", _Rest/binary>> -> true;
		_ -> false
	end.

get_heartbeat(Number) ->
	Beat = binary:list_to_bin([<<"\~h\~">>, integer_to_list(Number)]),
	encode(Beat).

ref_to_msg(Ref) ->
	socketio_utils:encode(binary:list_to_bin(erlang:ref_to_list(Ref))).

random() ->
	Now = {_, _, Micro} = now(),
 	Nowish = calendar:now_to_universal_time(Now),
 	Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
 	Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
 	Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
 	list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))).

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) ->
	to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
	[to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 ->
	$0 + N;
to_digit(N) ->
	$a + N-10.