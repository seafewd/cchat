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



% client requests to change nickname
handle(St, {nick, OldNick, NewNick}) ->
	case lists:member(NewNick, St#state.nicknames) of
		% OldNick same as NewNick
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
handle(St, {leave, Channel}) ->
	% set and send response atom
	Response = genserver:request(list_to_atom(Channel), {leave, Channel}),
	{reply, Response, St};

% handle client quit request
handle(St, {quit, ClientNick}) ->
	io:fwrite("Client has /quit. Deleting client from nickname list.\n"),
	% delete nickname from members list
	NewNicknameList = lists:delete(ClientNick, St#state.nicknames),
	{reply, ok, St#state{nicknames = NewNicknameList}};

% prepare to stop server. delete existing channels
handle(St, prepare_to_stop) ->
	io:fwrite("server shutting down...\n"),
	lists:foreach((fun(Channel) ->
		channel:delete(Channel) end),
	St#state.channels),
	{reply, ok, St};


% catch-all request
handle(St, _) ->
	{reply, {error, unrecognized_command, "The server can't handle this request. DEVS PLS."}, St}.


% Start a new server process with the given name
start(ServerAtom) ->
    genserver:start(ServerAtom, initialState(), fun handle/2).

% Stop the server process with the given name
stop(ServerAtom) ->
	genserver:request(ServerAtom, prepare_to_stop),
	genserver:stop(ServerAtom).
