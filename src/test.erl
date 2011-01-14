-module (test).

-export([ start/1, loop/1
        ]).

%% internal export (so hibernate can reach it)

-export ([handle_ws/1]).

-define(LOOP, {?MODULE, loop}).

start(Port) ->
		Options = [{port, Port}],
    mochiweb_http:start([{name, ?MODULE}, {loop, ?LOOP} | Options]).

loop(Req) ->
	{Bool, Version} = mochiweb_websocket_server:check(Req:get(headers)),
   case Bool of
   	true ->
			mochiweb_websocket_server:create_ws(Req, Version, true, fun(Ws)-> handle_ws(Ws) end);
		false ->
			io:format("No WS")
   end,
   ok.

handle_ws(WsClient) ->
  io:format("IN THE LOOP"),
  handle_ws(WsClient).