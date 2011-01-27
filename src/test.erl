-module (test).

-export([ start/1, incoming/1]).

-export ([handle_socketio/1]).

-define(INCOMING, {?MODULE, incoming}).

start(Port) ->
	Options = [{port, Port}],
	mochiweb_http:start([{name, ?MODULE}, {loop, ?INCOMING} | Options]).

incoming(Req) ->
  BinPath = binary:list_to_bin([Req:get(path)]),
  case BinPath of
		<<"/rt/", Rest/binary>> ->
			socketio:create(Rest, Req, true, {5000},
				fun(SocketIo)->
					handle_socketio(SocketIo)
				end);
		_ ->
			io:format("Not handled~n"),
			Req:ok("FAIL")
  end.

handle_socketio(SocketIo) ->
  receive
		open ->
			io:format("O HERRO!~n"),
			handle_socketio(SocketIo);
    {data, Data} ->
			SocketIo:send(Data),
      handle_socketio(SocketIo);
		gone ->
      handle_socketio(SocketIo)
  end.