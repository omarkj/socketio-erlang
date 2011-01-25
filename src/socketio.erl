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
-module (socketio).
-behaviour (gen_server).
-include ("socketio.hrl").
-define (SERVER, ?MODULE).

%% API
-export ([start_link/1, create/4, broadcast/1]).

%% Supervisor callback
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
start_link(Transports) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Transports], []).

broadcast(_) ->
	void.

create(<<"websocket">>, Req, AutoExit, Loop) ->
	case mochiweb_websocket_server:check(Req:get(headers)) of
		{true, Version} ->
			gen_event:add_handler(socketio_manager, {socketio_ws_handler, erlang:make_ref()},
				[Req, Version, true, Loop]);
		_ ->
			Req:ok("No WS")
	end.

%% gen_server callback
init([Transports]) ->
	gen_event:start_link({local, socketio_manager}),
	{ok, Transports}.

handle_call({ws, Req, Version, AutoExit, Loop}, _From, Transports) ->
	{reply, ok, Transports}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

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