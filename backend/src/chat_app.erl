 -module(chat_app).
 -behaviour(application).

 -export([start/2, stop/1]).

start(_Type, _Args) ->
    ChatRoom = chat_room:start(),
    Dispatch = cowboy_router:compile([
      {'_', [
        {<<"/ws">>, chat_ws_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [{port, 5555}], #{env => #{dispatch => Dispatch}}),
    io:format("Cowboy WebSocket listener started on port 5555~n"),
    {ok, self()}.

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
