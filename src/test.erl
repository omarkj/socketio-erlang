-module (test).

-export([ start/1, loop/1
        ]).

-export ([handle_ws/1]).

-define(LOOP, {?MODULE, loop}).

start(Port) ->
		Options = [{port, Port}],
    mochiweb_http:start([{name, ?MODULE}, {loop, ?LOOP} | Options]).

loop(Req) ->
	{Bool, Version} = mochiweb_websocket_server:check(Req:get(headers)),
   case Bool of
   	true ->
			mochiweb_websocket_server:create_ws(Req, Version, true,
			  fun(Ws)-> handle_ws(Ws) end);
		false ->
			io:format("No WS")
   end,
   ok.

handle_ws(WsClient) ->
  receive
    {data, Data} ->
      io:format("Data is here, and it is ~p~n", [Data]),
      handle_ws(WsClient);
    gone ->
      io:format("The client is gone");
    _ ->
      handle_ws(WsClient)
  after 2000 ->
    WsClient:send("Beat"),
    handle_ws(WsClient)
  end.