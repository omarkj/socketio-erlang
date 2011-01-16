% Ported from Misultin
% TODO: Refactor into a gen_server
-module (mochiweb_websocket_server).

-export ([check/1, create_ws/4]).

% External API
check(Headers) ->
	SupportedVersions = [{'draft-hixie', 76}, {'draft-hixie', 68}],
	mochiweb_ws_utils:check_websockets(SupportedVersions, Headers).

create_ws(Req, Version, AutoExit, Loop) ->
	Headers = Req:get(headers),
	case check(Headers) of
	  {true, Version} -> % Create WS client
	    Host = mochiweb_headers:get_value(host, Headers),
    	Origin = mochiweb_headers:get_value(origin, Headers),
    	Socket = Req:get(socket),

    	Ws = [
    		{version, Version}, {socket, Socket}, {scheme, Req:get(scheme)},
    		{path, Req:get(path)}, {headers, Headers}, {origin, Origin},
    		{host, Host}, {autoexit, AutoExit}
    	], % This should be a record, or I could change this to whatever Mochi uses by default

    	Handshake = mochiweb_ws_utils:create_handshake(
    		proplists:get_value(version, Ws),
    		Socket, Headers,
    		proplists:get_value(path, Ws),
    		Origin, Host),	

    	mochiweb_socket:send(Socket, Handshake),

    	WsClient = mochiweb_websocket:new(Ws, self()), % Create the client
    	WsLoop = spawn(fun() -> Loop(WsClient) end),

    	erlang:monitor(process, WsLoop),
    	mochiweb_socket:setopts(Socket, [{packet, 0}, {active, true}]), % on
    	
    	% Start the loop
    	websocket_loop(Socket, [], WsLoop, AutoExit);
	  _ ->
	    false
	end.

% The loop
websocket_loop(Socket, Buffer, WsLoop, AutoExit) ->
  receive
    {tcp, Socket, Data} -> % Incoming data
      handle_data(Buffer, binary_to_list(Data),
        Socket, WsLoop, AutoExit);
		{tcp_closed, Socket} -> % Socket closed
		  ws_close(Socket, WsLoop, AutoExit);
		{'DOWN', Ref, process, WsLoop, Reason} -> % Socket down. TODO: Add loggin
		  error_logger:error_report("Websocket down. Error: ~p~n",
		    [{Ref, Reason}]),
		  erlang:demonitor(WsLoop),
		  ws_close(Socket, WsLoop, AutoExit);
		{send, Data} -> % Outgoing data
		  mochiweb_socket:send(Socket, [0, Data, 255]),
		  websocket_loop(Socket, Buffer, WsLoop, AutoExit);
		shutdown -> % User asked to close socket
		  ws_close(Socket, WsLoop, AutoExit);
		_ -> % We don't care
		  websocket_loop(Socket, Buffer, WsLoop, AutoExit)
	end.

% Handle incoming data
handle_data(none, [0|T], Socket, WsLoop, AutoExit) ->
  handle_data([], T, Socket, WsLoop, AutoExit);
handle_data(none, [], Socket, WsLoop, AutoExit) ->
	websocket_loop(Socket, none, WsLoop, AutoExit);
handle_data(L, [255|T], Socket, WsLoop, AutoExit) ->
  D = lists:flatten(L),
	WsLoop ! {data, lists:reverse(D)},
	handle_data(none, T, Socket, WsLoop, AutoExit);
handle_data(L, [H|T], Socket, WsLoop, AutoExit) ->
	handle_data([H|L], T, Socket, WsLoop, AutoExit);
handle_data([], L, Socket, WsLoop, AutoExit) ->
	websocket_loop(Socket, L, WsLoop, AutoExit).

% Close the socket
ws_close(Socket, WsLoop, AutoExit) ->
  case AutoExit of
    true ->
      io:format("CYA!"),
      exit(WsLoop, kill); % Kill the loop
    _ ->
      WsLoop ! gone % Inform the loop that the connection has been closed
  end,
  mochiweb_socket:close(Socket).