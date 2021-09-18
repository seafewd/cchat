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

% handle client request to join a channel
handle(St, {join, ClientPid, ClientNick, Channel}) ->
	NewNicknameList =
		case lists:member(ClientNick, St#state.nicknames) of
			true	-> St#state.nicknames;
			false	-> [ClientNick | St#state.nicknames]
		end,
	NewChannelsList =
	 	case lists:member(Channel, St#state.channels) of
			true	-> St#state.channels;
			false	-> channel:start(Channel), [Channel | St#state.channels]
		end,
	Response = genserver:request(list_to_atom(Channel), {join, ClientPid}),
	{reply, Response, St#state{nicknames = NewNicknameList, channels = NewChannelsList}};

% handle client exit request
handle(St, {quit, ClientNick}) ->
	NewNicknameList = lists:delete(ClientNick, St#state.nicknames),
	{reply, ok, St#state{nicknames=NewNicknameList}};

% catch-all request
handle(St, _) ->
	{reply, {error, unrecognized_command, "The server can't handle this request. DEVS PLS."}}.


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    Server = genserver:start(ServerAtom, initialState(), fun handle/2),
	io:fwrite("cchat server started...\nProcess ID: ~p~n", [Server]).
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID

% Stop the server process registered to the given name,
stop(ServerAtom) ->
	genserver:stop(ServerAtom).
