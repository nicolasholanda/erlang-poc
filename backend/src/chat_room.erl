-module(chat_room).
-export([start/0, room_loop/1]).

start() ->
    RoomPid = spawn(chat_room, room_loop, [[]]),
    register(chat_room, RoomPid),
    RoomPid.

room_loop(Users) ->
    receive
        {join, Username, UserPid} ->
            NewUsers = [{Username, UserPid} | Users],
            io:format("~s joined~n", [Username]),
            lists:foreach(fun({_, P}) -> P ! {user_list, lists:map(fun({U,_}) -> U end, NewUsers)} end, NewUsers),
            room_loop(NewUsers);
        {leave, UserPid} ->
            NewUsers = lists:filter(fun({_, Pid}) -> Pid =/= UserPid end, Users),
            lists:foreach(fun({_, P}) -> P ! {user_list, lists:map(fun({U,_}) -> U end, NewUsers)} end, NewUsers),
            room_loop(NewUsers);
        {typing, Username, Typing} ->
            lists:foreach(fun({_, P}) -> P ! {typing, Username, Typing} end, Users),
            room_loop(Users);
        {message, FromUsername, Msg} ->
            Ts = erlang:system_time(millisecond),
            io:format("Broadcasting: ~s from ~s~n", [Msg, FromUsername]),
            lists:foreach(fun({_, P}) -> P ! {message, FromUsername, Msg, Ts} end, Users),
            room_loop(Users)
    end.
