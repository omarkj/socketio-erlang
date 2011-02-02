% Socket.IO server for Erlang
% 
% Copyright (C) 2011, Kóði ehf, Ómar Yasin <omar@kodiak.is>
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
-module (socketio_ws).
-include ("socketio.hrl").
-behaviour (gen_server).

%% API
-export([start/4, start_link/6, stop/0]).

%% gen_event callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(REF, ?MODULE).
-record (state, { req, socketioloop, socketpid, socketio, loop, timeout, version}).

%%% gen_event callbacks
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
start_link(Req, SessionId, Version, AutoExit, Options, Loop) ->
	{ok, Pid} = gen_server:start_link({global, SessionId}, ?MODULE,
		[Req, SessionId, Version, AutoExit, Options, Loop], []),
	gen_tcp:controlling_process(Req:get(socket), Pid),
	gen_server:cast(Pid, start).

start(Req, AutoExit, Options, Loop) ->
	case mochiweb_websocket_server:check(Req:get(headers)) of
		{true, Version} ->
		  SessionId = socketio_utils:random(),
			socketio_ws:start_link(Req, SessionId, Version, AutoExit, Options, Loop);
		_ ->
			Req:ok("No WS")
	end.

stop() ->
	gen_server:cast(?REF, stop).

%%% gen_event callbacks
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @spec init(Args) -> {ok, State}
init([Req, SessionId, Version, AutoExit, {Timeout}, Loop]) ->
	SocketIo = #socketio{
		type = websocket,
		unique_id = SessionId,
    scheme = Req:get(scheme),
    headers = Req:get(headers),
    autoexit = AutoExit
  },
	{ok, #state {req = Req,socketio = SocketIo, loop = Loop, timeout = Timeout,
		version = Version}}.

handle_cast(start, #state{socketio = SocketIo, req = Req, loop = Loop,
		timeout = Timeout, version = Version} = State) ->
	SessionId = State#state.socketio#socketio.unique_id,
	WsServerPid = self(),
	SocketIoClient = socketio_interface:new(SocketIo, self()),
	SocketIoLoop = spawn(fun() -> Loop(SocketIoClient) end),
	SocketPid = spawn(fun() -> mochiweb_websocket_server:create_ws(Req, Version, true, 
		fun(Ws) ->
			gen_server:cast(WsServerPid, {send, socketio_utils:encode(SessionId)}),
			gen_server:cast(WsServerPid, open),
			handle_websocket(Ws, WsServerPid, Timeout, 0)
		end)
	end),
	gen_tcp:controlling_process(Req:get(socket), SocketPid),
	monitor(process, SocketPid),
	{noreply, State#state{ socketioloop = SocketIoLoop, socketpid = SocketPid }};

handle_cast({data, Data}, State = #state{socketioloop = SocketIoLoop}) ->
	case socketio_utils:decode(Data, []) of
		Message ->
			lists:map(fun(M) ->
				SocketIoLoop ! {data, M}
			end, Message)
	end,
	{noreply, State};

handle_cast({send, Data}, State = #state{socketpid = SocketPid}) ->
	SocketPid ! {send, Data},
	{noreply, State};

handle_cast(open, State = #state{socketioloop = SocketIoLoop}) ->
	SocketIoLoop ! open,
	{noreply, State};

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_websocket(Ws, WsServerPid, Timeout, Counter) ->
	receive
		{data, Data} ->
			gen_server:cast(WsServerPid, {data, Data}),
			handle_websocket(Ws, WsServerPid, Timeout, Counter);
		_ ->
			handle_websocket(Ws, WsServerPid, Timeout, Counter)
		after Timeout ->
			NewCounter = Counter + 1,
			Heartbeat = socketio_utils:get_heartbeat(NewCounter),
			gen_server:cast(WsServerPid, {send, Heartbeat}),
			handle_websocket(Ws, WsServerPid, Timeout, NewCounter)
  end.

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
handle_info({'DOWN', _, process, _, _}, State) ->
	{stop, normal, State};
handle_info({'EXIT', _, process, _, _}, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, #state{socketio = SocketIo, socketioloop = SocketIoLoop,
	socketpid = SocketPid} = _State) ->
	case SocketIo#socketio.autoexit of
		true ->
			exit(SocketIoLoop, kill),
			exit(SocketPid, kill);
		_ ->
			SocketIoLoop ! gone,
			exit(SocketPid, kill)
	end,
	ok;
terminate(_, _) ->
	ok.

%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
