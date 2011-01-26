-module (socketio_utils).

-export ([encode/1, decode/1, get_heartbeat/1]).

% Encode messages to Socket.IO format
encode({json, Message}) ->
  encode(binary:list_to_bin([<<"\~j\~">>, Message]));
encode(Message) ->
  Len = binary:list_to_bin(integer_to_list(erlang:size(Message))), % Yuck
  binary:list_to_bin([<<"\~m\~">>, Len, <<"\~m\~">>, Message]).

% Messages look like:
% ~m~MSGLENGTH~m~MESSAGE or
% ~m~MSGLENGTH~m~~j~JSON
decode(<<"\~m\~", Rest/binary>>) ->
  case binary:match(Rest, <<"\~m\~">>) of
    nomatch -> badmsg; % Not valid, not sure what to do.
    {Beginning, End} ->
      Cons = Beginning + End,
      M = erlang:size(Rest) - Cons,
      binary:part(Rest, {Cons, M});
    _ ->
      badmsg
  end;
decode(Message) ->
  Message.

get_heartbeat(Number) ->
	binary:list_to_bin([<<"\~h\~">>, integer_to_list(Number)]).