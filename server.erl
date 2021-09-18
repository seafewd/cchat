-module(server).
-export([start/1,stop/1]).

-record(state, {
	nicknames,
	channels
}).

% initial state of server
initialState() ->
	#state{
		nicknames = [],
		channels = []
	}.

% handle client request to join a channel
handle(St, {join, ClientPid, ClientNick, Channel}) ->
	% prepend nick to list of nicks unless already member
	NewNicknameList =
		case lists:member(ClientNick, St#state.nicknames) of
			true	-> St#state.nicknames;
			false	-> [ClientNick | St#state.nicknames]
		end,
	% prepend channel to list of channels and create the process unless already exists
	NewChannelsList =
	 	case lists:member(Channel, St#state.channels) of
			true	-> St#state.channels;
			false	-> channel:create(Channel), [Channel | St#state.channels]
		end,
	% set response atom
	Response = genserver:request(list_to_atom(Channel), {join, ClientPid}),
	% send response and update new nicks and channels
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
