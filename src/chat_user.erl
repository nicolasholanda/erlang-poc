
-module(chat_user).
-export([start/2, user_loop/2]).

start(Username, RoomPid) ->
    UserPid =spawn(chat_user, user_loop, [Username, RoomPid]),
    RoomPid ! {join, Username, UserPid},
    UserPid.

user_loop(Username, RoomPid) ->
    receive
        {chat, FromPid, Msg} ->
            io:format("~s received message from ~s: ~s~n", [Username, FromPid, Msg]),
            user_loop(Username, RoomPid);
        {send_message, Msg} ->
            RoomPid ! {message, Username, Msg},
            user_loop(Username, RoomPid);
        leave ->
            RoomPid ! {leave, self()},
            io:format("~s has left the chat.~n", [Username])
    end.
