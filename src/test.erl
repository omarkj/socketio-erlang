-module (test).

-export([ start/1, incoming/1 ]).

-export ([handle_socketio/1]).

-define(INCOMING, {?MODULE, incoming}).

start(Port) ->
		Options = [{port, Port}],
    mochiweb_http:start([{name, ?MODULE}, {loop, ?INCOMING} | Options]).

incoming(Req) ->
  BinPath = binary:list_to_bin([Req:get(path)]),
  case BinPath of
    <<"/rt/", Rest/binary>> ->
      socketio:create_socketio(Rest, Req, true,
        fun(SocketIo) ->
          handle_socketio(SocketIo)
        end);
    _ ->
      io:format("Not handled~n")
  end.

handle_socketio(SocketIo) ->
  receive
    {data, Data} ->
      handle_socketio(SocketIo);
    gone ->
      io:format("The client is gone");
    _ ->
      handle_socketio(SocketIo)
  end.