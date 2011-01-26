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
-module (socketio_xhrpolling).
-include ("socketio.hrl").
-behaviour (gen_server).

%% API
-export([start_link/5, find_process/1]).

-export ([resume/4]).

%% gen_event callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define (LOOP, {?MODULE, xhr_loop}).
-define(REF, ?MODULE).

%%% gen_event callbacks
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
start_link(Req, Session, AutoExit, Options, Loop) ->
	gen_server:start_link({global, {sio_xp, Session}}, ?MODULE,
		[Req, AutoExit, Options, Loop], []).

find_process(Session) ->
	global:whereis_name({sio_xp, Session}).

%%% gen_event callbacks
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @spec init(Args) -> {ok, State}
init([Req, AutoExit, {_}, Loop]) ->
	SocketIo = #socketio{
    req = undefined, % Remove from the socket io object since it's not a given
		type = 'XHR polling',
    scheme = Req:get(scheme),
    path = undefined, % Remove
    headers = Req:get(headers),
    autoexit = AutoExit
  },
	SocketIoClient = socketio_interface:new(SocketIo, self()),
	SocketIoLoop = spawn(fun() -> Loop(SocketIoClient) end),
	gen_server:cast(self(), {req, Req}),
	{ok, {SocketIo, SocketIoLoop}}.

handle_cast({req, Req}, State) ->
	case Req:get(method) of
		'GET' ->
			xhr_loop(Req, 10000, 0);
		'POST' ->
			void;
		_ ->
			void
	end,
	{noreply, State};

handle_cast({data, Buffer}, {SocketIo, SocketIoLoop}) ->
	case socketio_utils:decode(Buffer) of
		heartbeat -> void;
		Message ->
			SocketIoLoop ! {data, Message}
	end,
	{noreply, {SocketIo, SocketIoLoop}};

handle_cast({send, Data}, {SocketIo, SocketIoLoop}) ->
	Req = SocketIo#socketio.req,
	mochiweb_websocket_server:send(Req:get(socket), Data),
	{noreply, {SocketIo, SocketIoLoop}};

handle_cast(open, {SocketIo, SocketIoLoop}) ->
	SocketIoLoop ! open,
	{noreply, {SocketIo, SocketIoLoop}};

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

xhr_loop(Req, Timeout, Counter) ->
	Reentry = mochiweb_http:reentry(?LOOP),
	NewCounter = Counter + 1,
	Hearbeat = socketio_utils:get_heartbeat(NewCounter),
	erlang:send_after(Timeout, self(), "Hearbeat"), % This is the heartbeat
	proc_lib:hibernate(?MODULE, resume, [Req, Reentry, Timeout, Counter]),
	ok.

resume(Req, Reentry, Timeout, Counter) ->
	receive
		Outgoing ->
			Req:ok({_ContentType = "text/plain",
	            _Headers = [
								{"Access-Control-Allow-Origin", "*"}
							],
	            "Response"})
	end,
	io:format("About to reenter loop"),
	io:format("Reentry is ~p~n", [Reentry]),
	Reentry(Req, Timeout, Counter).

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
