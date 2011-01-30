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
-module (socketio_xhrpolling).
-include ("socketio.hrl").
-behaviour (gen_server).

%% API
-export([start/5, start_link/5, find_process/1, check_living/1]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(REF, ?MODULE).
-record (state, {buffer = [], xhrpid, socketioloop,
	timeout, timerref, autoexit}).

%%% gen_server callbacks
%% Creates xhr poll server
start_link(Req, Session, AutoExit, Options, Loop) ->
	gen_server:start_link({global, {sio_xp, Session}}, ?MODULE,
		[Req, Session, AutoExit, Options, Loop], []).

start(Path, Req, AutoExit, Options, Loop) ->
  [Session|Tail] = binary:split(Path, <<"/">>),
	case binary:referenced_byte_size(Session) of
		0 -> % No session, create new session
			SessionId = socketio_utils:random(),
			socketio_xhrpolling:start_link(Req, SessionId, AutoExit, Options, Loop);
		_ -> % Some session, look it up and pid it
			case socketio_xhrpolling:find_process(Session) of
				undefined ->
					Req:ok({_ContentType = "text/plain",
						_Headers = [{"Access-Control-Allow-Origin", "*"},
						{"Connection", "keep-alive"}], "error"}); % Probably not what SocketIO expects
				Pid ->
					case Tail of
						[<<"send">>] ->
							Incoming = case Req:recv_body() of
								<<"data=", Data/binary>> ->
									Data;
								_ ->
									<<>>
							end,
							gen_server:cast(Pid, {data, Req, Incoming});
						_ ->
							gen_server:cast(Pid, {poll, Req})
					end
			end
	end.

find_process(Session) ->
	global:whereis_name({sio_xp, Session}).

check_living(Pid) ->
	gen_server:cast(Pid, check).

%% Callback
init([Req, Session, AutoExit, {Timeout}, Loop]) ->
	SocketIo = #socketio{
		type = 'XHR polling',
    scheme = Req:get(scheme),
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
	case socketio_utils:decode(Body, []) of
		heartbeat -> void;
		Message ->
			lists:map(fun(M) ->
				case mochiweb_util:parse_qs(M) of
					[{Data, _}|_] ->
						SocketIoLoop ! {data, list_to_binary(Data)};
					_ -> void
				end
			end, Message)
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

xhr_loop(Req, XhrPollingPid, Timeout) -> % TODO: Make this loop hibernate
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

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{socketioloop = SocketIoLoop, autoexit = AutoExit}) ->
	case AutoExit of
		true ->
			exit(SocketIoLoop, kill);
		_ ->
			SocketIoLoop ! gone
	end,
	ok;
terminate(_, _) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
