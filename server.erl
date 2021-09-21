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
			true  -> St#state.nicknames;
			false -> [ClientNick | St#state.nicknames]
		end,
	% prepend channel to list of channels and create the process unless already exists
	NewChannelsList =
	 	case lists:member(Channel, St#state.channels) of
			true  -> St#state.channels;
			false ->
				channel:create(Channel),
				[Channel | St#state.channels]
		end,
	% set response atom
	Response = genserver:request(list_to_atom(Channel), {join, ClientPid}),
	% send response and update new nicks and channels
	{reply, Response, St#state{nicknames = NewNicknameList, channels = NewChannelsList}};

% client requests to send a message
handle(St, {message_send, ClientPid, ClientNick, Channel, Msg}) ->
	Request = genserver:request(list_to_atom(Channel), {message_send, ClientNick, ClientPid, Msg}),
	case Request of
		ok -> {reply, Request, St};
		Error -> {error, Error, St}
	end;

% client requests to change nickname
handle(St, {nick, OldNick, NewNick}) ->
	case lists:member(NewNick, St#state.nicknames) of
		% OldNick == NewNick
		true when OldNick =:= NewNick -> {reply, ok, St};
		% nickname is already in use by another client
		true -> {reply, {error, nick_taken, "Nickname " ++ NewNick ++ " is already taken. Try another."}, St};
		% client updates with new username
		false ->
			% delete old nickname, add new nickname
			NewNicksList = [NewNick | lists:delete(OldNick, St#state.nicknames)],
			{reply, ok, St#state{nicknames = NewNicksList}}
	end;

% handle client leaves channel
handle(St, {leave, ClientPid, Channel}) ->
	% set and send response atom
	Response = genserver:request(list_to_atom(Channel), {leave, ClientPid}),
	{reply, Response, St};

% handle client exit request
handle(St, {quit, ClientNick}) ->
	% delete nickname from members list
	NewNicknameList = lists:delete(ClientNick, St#state.nicknames),
	{reply, ok, St#state{nicknames=NewNicknameList}};

% catch-all request
handle(St, _) ->
	{reply, {error, unrecognized_command, "The server can't handle this request. DEVS PLS."}, St}.


% Start a new server process with the given name
start(ServerAtom) ->
    genserver:start(ServerAtom, initialState(), fun handle/2).

% Stop the server process with the given name
stop(ServerAtom) ->
	genserver:stop(ServerAtom).
