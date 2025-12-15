-module(chat_room).
-export([start/0, room_loop/1]).

start() ->
    RoomPid = spawn(chat_room, room_loop, [[]]).

room_loop(Users) ->
    receive
        {join, Username, UserPid} ->
            NewUsers = [{Username, UserPid} | Users],
            io:format("~s joined~n", [Username]),
            room_loop(NewUsers);
        {leave, UserPid} ->
            NewUsers = lists:filter(fun({_, Pid}) -> Pid =/= UserPid end, Users),
            room_loop(NewUsers);
        {message, FromUsername, Msg} ->
            io:format("Broadcasting: ~s from ~s~n", [Msg, FromUsername]),
            lists:foreach(fun({_, P}) -> P ! {chat, FromUsername, Msg} end, Users),
            room_loop(Users)
    end.
