-module (test).

-export([ start/1, incoming/1]).

-export ([handle_socketio/1]).

-define(INCOMING, {?MODULE, incoming}).

start(Port) ->
	Options = [{port, Port}],
	mochiweb_http:start([{name, ?MODULE}, {loop, ?INCOMING} | Options]),
	gen_event:start_link({local, socketio_manager}).

incoming(Req) ->
  BinPath = binary:list_to_bin([Req:get(path)]),
  case BinPath of
		<<"/rt/", Rest/binary>> ->
			socketio:create(Rest, Req, true,
				fun(SocketIo)->
					SocketIo:send("Welcome!"),
					handle_socketio(SocketIo)
				end);
		_ ->
			io:format("Not handled~n"),
			Req:ok("FAIL")
  end.

handle_socketio(SocketIo) ->
  receive
    {data, Data} ->
			io:format("Got data ~p~n", [Data]),
			SocketIo:send(Data),
      handle_socketio(SocketIo);
		{client, gone} ->
			io:format("The client is gone"),
      handle_socketio(SocketIo)
  after 2000 ->
    SocketIo:send("Beat"),
    handle_socketio(SocketIo)
  end.