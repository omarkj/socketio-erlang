% Socket.IO server for Erlang
% 
% Copyright (C) 2011, Kóði ehf <info@kodiak.is>, Ómar Yasin <omar@kodiak.is>
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
-export([start_link/5, find_process/1, check_living/1]).

%% gen_event callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(REF, ?MODULE).
-record (state, {buffer = [], xhrpid, socketioloop,
	timeout, timerref, autoexit}).

%%% gen_event callbacks
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
start_link(Req, Session, AutoExit, Options, Loop) ->
	gen_server:start_link({global, {sio_xp, Session}}, ?MODULE,
		[Req, Session, AutoExit, Options, Loop], []).

find_process(Session) ->
	global:whereis_name({sio_xp, Session}).

check_living(Pid) ->
	gen_server:cast(Pid, check).

%% Gen_server
init([Req, Session, AutoExit, {Timeout}, Loop]) ->
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
	Req:ok({_ContentType = "text/plain", % Return the session ID to the user
		_Headers = [
			{"Access-Control-Allow-Origin", "*"},
			{"Connection", "keep-alive"}], socketio_utils:encode(Session)}),
	gen_server:cast(self(), open),
	{ok, #state{ socketioloop = SocketIoLoop, timeout = Timeout, autoexit = AutoExit }}.

handle_cast(open, State = #state{socketioloop = SocketIoLoop}) ->
	SocketIoLoop ! open,
	{noreply, State};

handle_cast({poll, Req}, State = #state{timeout = Timeout}) ->
	NewState = case Req:get(method) of
		'GET' ->
			XhrServerPid = self(),
			XhrPid = spawn(fun() -> xhr_loop(Req, XhrServerPid, Timeout) end),
			gen_server:cast(self(), flush),
			State#state{xhrpid = XhrPid};
		_ ->
			State
	end,
	{noreply, NewState};

handle_cast({data, Req, Body}, State = #state{socketioloop = SocketIoLoop}) ->
	case socketio_utils:decode(Body) of
		heartbeat -> void;
		Message ->
			[{Data, _}|_] = mochiweb_util:parse_qs(Message), % Getting this encoded
			SocketIoLoop ! {data, list_to_binary(Data)}
	end,
	Req:ok({_ContentType = "text/plain",
		_Headers = [{"Access-Control-Allow-Origin", "*"},
		{"Connection", "keep-alive"}], "ok"}),
	{noreply, State};

handle_cast({send, Data}, State = #state{xhrpid = XHRPid, buffer = Buffer}) ->
	NewState = case XHRPid of
		undefined ->
			NewBuffer = Buffer ++ [Data],
			State#state{buffer = NewBuffer};
		Pid ->
			Pid ! {send, Data},
			State
	end,
	{noreply, NewState};

handle_cast(flush, State = #state{buffer = Buffer, xhrpid = XhrPid}) ->
	NewState = case length(Buffer) of
		0 -> State;
		_ ->
			io:format("Flush the buffer of length ~p~n", [length(Buffer)]),
			XhrPid ! {send, Buffer},
			State#state{buffer = []}
	end,
	{noreply, NewState};

handle_cast(open, State) ->
	State#state.socketioloop ! open,
	{noreply, State};

handle_cast(gone, State = #state{timeout = Timeout, timerref = TimerRef}) -> % User not here
	case TimerRef of
		undefined -> void;
		TRef -> timer:cancel(TRef)
	end,
	WaitFor = Timeout + 2000,
	{ok, NewTimer} = timer:apply_after(WaitFor, ?MODULE, check_living, [self()]),
	{noreply, State#state{xhrpid = undefined, timerref = NewTimer}};

handle_cast(gone, State) -> % User not here
	io:format("Matched this gone"),
	{noreply, State};

handle_cast(check, State = #state{xhrpid = XHRPid}) ->
	case XHRPid of
		undefined ->
			{stop, normal, State};
		_ ->
			{noreply, State}
	end;

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_Request, _From, State) ->
	Reply = ok,
  {reply, Reply, State}.

xhr_loop(Req, XhrPollingPid, Timeout) ->
	io:format("In loop~n"),
	receive
		{send, Message} ->
			gen_server:cast(XhrPollingPid, gone),
			Req:ok({_ContentType = "text/plain",
				_Headers = [{"Access-Control-Allow-Origin", "*"},
				{"Connection", "keep-alive"}], Message});
		_ -> void
		after Timeout ->
			gen_server:cast(XhrPollingPid, gone),
			Req:ok({_ContentType = "text/plain",
				_Headers = [{"Access-Control-Allow-Origin", "*"},
				{"Connection", "keep-alive"}], ""})
	end,
	ok.

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
	{noreply, State}.

%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, #state{socketioloop = SocketIoLoop, autoexit = AutoExit}) ->
	io:format("CYA"),
	case AutoExit of
		true ->
			exit(SocketIoLoop, kill);
		_ ->
			SocketIoLoop ! gone
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
