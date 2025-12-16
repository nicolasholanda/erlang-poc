-module(chat_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #{username => undefined}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Bin}, State) when is_binary(Bin) ->
    case catch jsx:decode(Bin, [return_maps]) of
        {'EXIT', _} -> {ok, State};
        Data ->
            Type = maps:get(<<"type">>, Data, undefined),
            case Type of
                <<"join">> ->
                    UsernameBin = maps:get(<<"username">>, Data),
                    Username = binary_to_list(UsernameBin),
                    chat_room ! {join, Username, self()},
                    NewState = State#{username => Username},
                    {ok, NewState};
                <<"message">> ->
                    UsernameBin = maps:get(<<"username">>, Data),
                    TextBin = maps:get(<<"text">>, Data),
                    Username = binary_to_list(UsernameBin),
                    Text = binary_to_list(TextBin),
                    chat_room ! {message, Username, Text},
                    {ok, State};
                <<"typing">> ->
                    UsernameBin = maps:get(<<"username">>, Data),
                    Typing = maps:get(<<"typing">>, Data),
                    Username = binary_to_list(UsernameBin),
                    chat_room ! {typing, Username, Typing},
                    {ok, State};
                _ -> {ok, State}
            end
    end;
websocket_handle(_Other, State) ->
    {ok, State}.

websocket_info({user_list, Users}, State) ->
    UsersBin = [list_to_binary(U) || U <- Users],
    Payload = #{type => <<"user_list">>, users => UsersBin},
    Bin = jsx:encode(Payload),
    {reply, {text, Bin}, State};
websocket_info({message, From, Text, Ts}, State) ->
    Payload = #{type => <<"message">>, from => list_to_binary(From), text => list_to_binary(Text), ts => Ts},
    Bin = jsx:encode(Payload),
    {reply, {text, Bin}, State};
websocket_info({typing, Username, Typing}, State) ->
    Payload = #{type => <<"typing">>, username => list_to_binary(Username), typing => Typing},
    Bin = jsx:encode(Payload),
    {reply, {text, Bin}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    chat_room ! {leave, self()},
    ok.
