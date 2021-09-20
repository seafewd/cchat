-module(channel).
-export([create/1, delete/1]).

% record with channel name and current members
-record(ch_state, {
    channelName,
    members
}).

% initial state of channel, created with its name and emtpy list of members
initial_state(Name) ->
    #ch_state{
        channelName = Name,
        members = []
    }.

% handle methods for different client requests

% clients requests to join a channel
handle(St, {join, ClientPid}) ->
    case lists:member(ClientPid, St#ch_state.members) of
        % user has already joined this channel
        true ->
            {reply, {error, user_already_joined, "You're already in channel " ++ St#ch_state.channelName}, St};
        % user hasn't joined - prepend client's name to members list and send back reply
        false ->
            NewMembersList = [ClientPid | St#ch_state.members],
            {reply, ok, St#ch_state{members = NewMembersList}}
    end;

% client requests to leave a channel
handle(St, {leave, ClientPid}) ->
    case lists:member(ClientPid, St#ch_state.members) of
        % user is a member of this channel - update state with new list
        true ->
            NewMembersList = lists:delete(ClientPid, St#ch_state.members),
            {reply, ok, St#ch_state{members = NewMembersList}};
        % user isn't in this channel, can't leave it
        false ->
            {reply, {error, user_not_joined, "You're not in channel " ++ St#ch_state.channelName}, St}
    end;

% handle client message send
handle(St, {message_send, ClientPid, ClientNick, Msg}) ->
    % don't send the message to ourselves
    Recipients = lists:delete(ClientPid, St#ch_state.members),
    Data = {request, self(), make_ref(), {message_receive, St#ch_state.channelName, ClientNick, Msg}},
    lists:foreach((fun(Member) -> Member ! Data end), Recipients),
    {reply, ok, St};

% catch-all handle method
handle(St, _) ->
    {reply, {error, not_implemented, "This channel can't handle this request."}, St}.

create(Name) ->
    genserver:start(list_to_atom(Name), initial_state(Name), fun handle/2).

delete(Name) ->
    genserver:stop(list_to_atom(Name)).
