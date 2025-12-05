-module(pingpong).
-export([start/0, ping/1, pong/0]).

start() ->
    PidPong = spawn(pingpong, pong, []),
    PidPing = spawn(pingpong, ping, [PidPong]),
    PidPing ! start.

ping(PidPong) ->
    receive
        start ->
            io:format("Ping~n"),
            PidPong ! {ping, self()},
            ping(PidPong);
        pong ->
            io:format("Ping~n"),
            PidPong ! {ping, self()},
            ping(PidPong)
    end.

pong() ->
    receive
        {ping, PidPing} ->
            io:format("Pong~n"),
            PidPing ! pong,
            pong()
    end.