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

% send a general request to destination (server) with try/catch for exception handling
send(Destination, Request) ->
    try genserver:request(Destination, Request) of
        Response -> Response
    catch
        error:_ -> {error, server_not_reached, "Can't reach server."}
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

% send leave channel request
handle(St, {leave, Channel}) ->
    case send(St#client_st.server, {leave, self(), Channel}) of
        ok ->
            NewChannelsList = lists:delete(Channel, St#client_st.channels),
            {reply, ok, St#client_st{channels = NewChannelsList}};
        Error ->
            {reply, Error, St}
        end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: check message length and if 0, dont send
    % ...
    % send message_send request to server
    Request = send(St#client_st.server, {message_send, self(), St#client_st.nick, Channel, Msg}),
    case Request of
        ok ->
            {reply, Request, St};
        Error -> {reply, Error, St}
    end;

% change nickname
handle(St, {nick, NewNick}) ->
    case send(St#client_st.server, {nick, St#client_st.nick, NewNick}) of
        % all good, change nickname
        ok -> {reply, ok, St#client_st{nick = NewNick}};
        % error, return state
        Error -> {reply, Error, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
