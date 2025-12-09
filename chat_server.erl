-module(chat_server).
-export([start/1, accept_loop/2, client_handler/2]).

start(Port) ->
    RoomPid = chat_room:start(),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]),
    io:format("Chat server started on port ~p~n", [Port]),
    accept_loop(ListenSocket, RoomPid).

accept_loop(ListenSocket, RoomPid) ->
    io:format("Waiting for connection...~n"),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Client connected! Socket: ~p~n", [Socket]),
    client_handler(Socket, RoomPid),
    accept_loop(ListenSocket, RoomPid).

client_handler(Socket, RoomPid) ->
    gen_tcp:send(Socket, <<"Enter username: ">>),
    receive
        {tcp, Socket, UsernameBin} ->
            Username = string:trim(binary_to_list(UsernameBin)),
            RoomPid ! {join, Username, self()},
            io:format("~s connected~n", [Username]),
            client_loop(Socket, RoomPid, Username)
    end.

client_loop(Socket, RoomPid, Username) ->
    receive
        {tcp, Socket, Data} ->
            Msg = string:trim(binary_to_list(Data)),
            RoomPid ! {message, Username, Msg},
            client_loop(Socket, RoomPid, Username);
        {chat, From, Msg} ->
            gen_tcp:send(Socket, io_lib:format("~s: ~s~n", [From, Msg])),
            client_loop(Socket, RoomPid, Username);
        {tcp_closed, Socket} ->
            RoomPid ! {leave, self()},
            io:format("~s disconnected~n", [Username])
    end.