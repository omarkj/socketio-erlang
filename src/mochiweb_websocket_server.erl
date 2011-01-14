% Ported from Misultin

-module (mochiweb_websocket_server).

-export ([check/1, create_ws/4]).

% External API
check(Headers) ->
	SupportedVersions = [{'draft-hixie', 76}, {'draft-hixie', 68}],
	check_websockets(SupportedVersions, Headers).

create_ws(Req, Version, Autoexit, Loop) ->
	Headers = Req:get(headers),
	Host = mochiweb_headers:get_value(host, Headers),
	Origin = mochiweb_headers:get_value(origin, Headers),
	Socket = Req:get(socket),
	
	Ws = [
		{version, Version}, {socket, Socket}, {scheme, Req:get(scheme)},
		{path, Req:get(path)}, {headers, Headers}, {origin, Origin},
		{host, Host}, {autoexit, Autoexit}
	], % This should be a record, or I could change this to whatever Mochi uses by default
	
	Handshake = create_handshake(
		proplists:get_value(version, Ws),
		Socket,
		Headers,
		proplists:get_value(path, Ws),
		Origin,
		Host),	
	
	mochiweb_socket:send(Socket, Handshake),
	
	WsClient = mochiweb_websocket:new(Ws, self()), % Create the client
	WsLoop = spawn(fun() -> Loop(WsClient) end),
	erlang:monitor(process, WsClient),
	mochiweb_socket:setopts(Socket, [{packet, 0}, {active, true}]), % on
	void.

%% ------------
%% Internal API
%% ------------
%% Start to check if WS is supported
check_websockets([], _) -> false;

check_websockets([Version|Rest], Headers) ->
	case check_websocket(Version, Headers) of
		false ->
			check_websockets(Rest, Headers);
		{true, _} ->
			{true, Version}
	end.

%% WS Header check logic
check_websocket({'draft-hixie', 76} = Version, Headers) ->
	RequiredHeaders = [
		{upgrade, "WebSocket"}, {connection, "Upgrade"}, {host, ignore}, {origin, ignore},
		{'sec-websocket-key1', ignore}, {'sec-websocket-key2', ignore}
	],
	case loop_through_headers(Headers, RequiredHeaders) of
		true ->
			{true, Version};
		_ ->
			false
	end;
	
check_websocket({'draft-hixie', 68} = Version, Headers) ->
	RequiredHeaders = [
		{upgrade, "WebSocket"}, {connection, "Upgrade"}, {host, ignore},
		{origin, ignore}
	],
	case loop_through_headers(Headers, RequiredHeaders) of
		true -> {true, Version};
		_ -> false
	end.

loop_through_headers(Headers, RequiredHeaders) ->
	lists:all(
		fun({Key, Value}) ->
			case mochiweb_headers:get_value(Key, Headers) of
				HeaderValue ->
					case Value of
						ignore -> true; % Ignoring it
						HeaderValue -> true; % Okay, this is a match
						_ -> false % Returns false, these headers don't match 
					end
			end
		end, RequiredHeaders).

%% Create the handshake logic
create_handshake({'draft-hixie', 76}, Socket, Headers, Path, Origin, Host) ->
	SecKey1 = mochiweb_headers:get_value('sec-websocket-key1', Headers),
	SecKey2 = mochiweb_headers:get_value('sec-websocket-key2', Headers),
	mochiweb_socket:setopts(Socket, [{packet, raw}, {active, false}]),
	Body = case mochiweb_socket:recv(Socket, 8, 30*1000) of
		{ok, Bin} ->
			Bin;
		{error, timeout} ->
			<<>>; % ERROR: Timeout
		_ ->
			<<>> % ERROR: Dunno
	end,
	["HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Origin: ", Origin, "\r\n",
		"Sec-WebSocket-Location: ws://", lists:concat([Host, Path]), "\r\n\r\n",
		build_challenge({'draft-hixie', 76}, SecKey1, SecKey2, Body)
	];
create_handshake({'draft-hixie', 68}, _, _, Path, Origin, Host) ->
	["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"WebSocket-Origin: ", Origin , "\r\n",
		"WebSocket-Location: ws://", lists:concat([Host, Path]), "\r\n\r\n"
	].

build_challenge({'draft-hixie', 76}, SecKey1, SecKey2, Body) ->
	Ikey1 = [D || D <- SecKey1, $0 =< D, D =< $9],
	Ikey2 = [D || D <- SecKey2, $0 =< D, D =< $9],
	Blank1 = length([D || D <- SecKey1, D =:= 32]),
	Blank2 = length([D || D <- SecKey2, D =:= 32]),
	Part1 = list_to_integer(Ikey1) div Blank1,
	Part2 = list_to_integer(Ikey2) div Blank2,
	Ckey = <<Part1:4/big-unsigned-integer-unit:8, Part2:4/big-unsigned-integer-unit:8, Body/binary>>,
	erlang:md5(Ckey).