-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % list of channels in use by client
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        nick = Nick,
        gui = GUIAtom,
        server = ServerAtom,
        channels = []
    }.

% send a general request to destination
send(Destination, Request) ->
    try genserver:request(Destination, Request) of
        Response -> Response
    catch
        error:_ -> {error, server_unreachable, "Server unreachable."}
    end.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
% send a join request to server with this Pid, client nick and channel
handle(St, {join, Channel}) ->
    case send(St#client_st.server, {join, self(), St#client_st.nick, Channel}) of
        ok ->
            % prepend new channel name to channel list and update client's list of channels
            NewChannelsList = [Channel | St#client_st.channels],
            {reply, ok, St#client_st{channels = NewChannelsList}};
        Error ->
            {reply, Error, St}
        end;

% Leave channel
handle(St, {leave, Channel}) ->
    case send(St#client_st.server, {leave, self(), Channel}) of
        ok ->
            {reply, ok, St};
        Error ->
            {reply, Error, St}
        end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case lists:member(Channel, St#client_st.channels) of
        true ->
            Reply = send(list_to_atom(Channel), {message_send, self(), St#client_st.nick, Msg}),
            {reply, Reply, St};
        false ->
            {reply, {error, user_not_joined, "User isn't a member of " ++ Channel ++ " ."}}
    end;

% change nickname
handle(St, {change_nick, NewNick}) ->
    case send(St#client_st.server, {change_nick, St#client_st.nick, NewNick}) of
        ok -> {reply, ok, St#client_st{nick = NewNick}};
        Error -> {reply, Error, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
