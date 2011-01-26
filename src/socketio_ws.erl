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
-module (socketio_ws).
-include ("socketio.hrl").
-behaviour (gen_server).

%% API
-export([start_link/5, stop/0]).

%% gen_event callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(REF, ?MODULE).

%%% gen_event callbacks
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
start_link(Req, Version, AutoExit, Options, Loop) ->
	gen_server:start_link({global, erlang:make_ref()}, ?MODULE,
		[Req, Version, AutoExit, Options, Loop], []).

stop() ->
	gen_server:cast(?REF, stop).

%%% gen_event callbacks
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @spec init(Args) -> {ok, State}
init([Req, Version, AutoExit, {Timeout}, Loop]) ->
	SocketIo = #socketio{
    req = Req,
		type = websocket,
    scheme = Req:get(scheme),
    path = Req:get(path),
    headers = Req:get(headers),
    autoexit = AutoExit
  },
	WsServerPid = self(),
	SocketIoClient = socketio_interface:new(SocketIo, self()),
	SocketIoLoop = spawn(fun() -> Loop(SocketIoClient) end),
	SocketPid = spawn(fun() -> create_socket(SocketIo#socketio.req, Version, WsServerPid, Timeout) end),
	monitor(process, SocketPid),
	{ok, {SocketIo, SocketIoLoop, SocketPid}}.
	
handle_cast({data, Buffer}, {SocketIo, SocketIoLoop, SocketPid}) ->
	case socketio_utils:decode(Buffer) of
		heartbeat -> void;
		Message ->
			SocketIoLoop ! {data, Message}
	end,
	{noreply, {SocketIo, SocketIoLoop, SocketPid}};

handle_cast({send, Data}, {SocketIo, SocketIoLoop, SocketPid}) ->
	Req = SocketIo#socketio.req,
	mochiweb_websocket_server:send(Req:get(socket), Data),
	{noreply, {SocketIo, SocketIoLoop, SocketPid}};

handle_cast(open, {SocketIo, SocketIoLoop, SocketPid}) ->
	SocketIoLoop ! open,
	{noreply, {SocketIo, SocketIoLoop, SocketPid}};

handle_cast(gone, {SocketIo, SocketIoLoop, SocketPid}) ->
	{noreply, {SocketIo, SocketIoLoop, SocketPid}};

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

create_socket(Req, Version, WsServerPid, Timeout) ->
	mochiweb_websocket_server:create_ws(Req, Version, true, 
		fun(Ws) ->
			gen_server:cast(WsServerPid, {send, socketio_utils:encode(binary:list_to_bin(erlang:ref_to_list(make_ref())))}),
			gen_server:cast(WsServerPid, open),
			handle_websocket(Ws, WsServerPid, Timeout, 0)
		end).

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
terminate(_Reason, {SocketIo, SocketIoLoop, SocketPid}) ->
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
