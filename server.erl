-module(server).
-export([start/1,stop/1]).

-record(state, {
	nicknames,
	channels
}).


initialState() ->
	#state{
		nicknames = [],
		channels = []
	}.

handle(State, {quit, ClientNick}) ->
	NewNicknameList = lists:delete(ClientNick, State#state.nicknames),
	{reply, ok, State#state{nicknames=NewNicknameList}};

handle(State, _) ->
	{reply, {error, unrecognized_command, "The server can't handle this request. DEVS PLS."}}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, initialState(), fun handle/2).
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID

% Stop the server process registered to the given name,
stop(ServerAtom) -> 
	genserver:stop(ServerAtom).
