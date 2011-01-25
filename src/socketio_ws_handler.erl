% Socket.IO server for Erlang
% 
% Copyright (C) 2011, Ómar Yasin <omar@kodiak.is>
% 
% All rights reserved.
% 
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
% 
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%    following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%    the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%    products derived from this software without specific prior written permission.
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
-module (socketio_ws_handler).
-include ("socketio.hrl").
-behaviour (gen_event).

%% API
-export([start_link/0, add_handler/0]). 

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
handle_info/2, terminate/2, code_change/3]).

-define(REF, ?MODULE).

%%% gen_event callbacks
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
start_link() ->
    gen_event:start_link({local, ?REF}).
    
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
add_handler() ->
    gen_event:add_handler(?REF, ?MODULE, []).

%%% gen_event callbacks
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @spec init(Args) -> {ok, State}
init([Req, Version, true, Loop]) ->
	SocketIo = #socketio{
    req = Req,
		type = websocket,
    scheme = Req:get(scheme),
    path = Req:get(path),
    headers = Req:get(headers),
    autoexit = true
  },
	SocketIoClient = socketio_interface:new(SocketIo, self()),
	SocketIoLoop = spawn(fun() -> Loop(SocketIoClient) end),
	gen_event:notify(self(), {open, Version}),
	{ok, {SocketIo, SocketIoLoop}}.

handle_event({open, Version}, {SocketIo, SocketIoLoop}) ->
	Self = self(),
	SocketServer = spawn(fun() -> create_socket(SocketIo#socketio.req, Version, true, Self) end),
	{ok, {SocketIo, SocketIoLoop}};
	
handle_event({data, Buffer}, {SocketIo, SocketIoLoop}) ->
	SocketIoLoop ! {data, Buffer},
	io:format("Got data ~p~n", [Buffer]),
	{ok, {SocketIo, SocketIoLoop}};

handle_event({send, Data}, {SocketIo, SocketIoLoop}) ->
	Req = SocketIo#socketio.req,
	Res = mochiweb_socket:send(Req:get(socket), Data),
	io:format("Result is ~p~n", [Res]),
	{ok, {SocketIo, SocketIoLoop}}.

create_socket(Req, Version, AutoExit, EventHandler) ->
	mochiweb_websocket_server:create_ws(Req, Version, true, 
		fun(Ws) ->
			handle_websocket(Ws, EventHandler)
		end).

handle_websocket(Ws, EventHandler) ->
	receive
    {data, Data} ->
			gen_event:notify(EventHandler, {data, Data}),
			handle_websocket(Ws, EventHandler);
		{send, Data} ->
			io:format("yo!"),
			handle_websocket(Ws, EventHandler);
		_ ->
			handle_websocket(Ws, EventHandler)
  end.

%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
handle_info(_Info, State) ->
    {ok, State}.

%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
