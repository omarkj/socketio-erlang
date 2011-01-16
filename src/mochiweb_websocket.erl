-module (mochiweb_websocket, [Ws, Pid]).

-export ([get/1, send/1, list/0]).

%% Module API
list() -> Ws.

get(Key) ->
  case proplists:get_value(Key) of
    undefined -> undefined;
    Match -> Match
  end.

send(Message) ->
  Pid ! {send, Message}.