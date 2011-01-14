-module (test).

-export([ start/1, loop/1
        ]).

%% internal export (so hibernate can reach it)
-export([ resume/3
        ]).

-define(LOOP, {?MODULE, loop}).

start(Port) ->
		Options = [{port, Port}],
    mochiweb_http:start([{name, ?MODULE}, {loop, ?LOOP} | Options]).

loop(Req) ->
	{Bool, Version} = mochiweb_websocket:check(Req:get(headers)),
	io:format("Is this websockets? ~p~n", [Bool]),
   case Bool of
   	true ->
			Ws = mochiweb_websocket:create_ws(Req, Version, true),
			io:format("WS is ~p~n", [Ws]);
		false ->
			io:format("No WS")
   end,
   ok.

%% this is the function that's called when a message arrives.
resume(Req, RestOfPath, Reentry) ->
    receive
        Msg ->
            Text = io_lib:format("wake up message: ~p~nrest of path: ~p", [Msg, RestOfPath]),
            ok(Req, Text)
    end,

    %% if we didn't call @Reentry@ here then the function would finish and the
    %% process would exit.  calling @Reentry@ takes care of returning control
    %% to @mochiweb_http@
    io:format("reentering loop via continuation in ~p~n", [Req:get(path)]),
    Reentry(Req).

ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).
