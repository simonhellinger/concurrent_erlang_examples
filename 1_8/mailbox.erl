-module(mailbox).
-export([receiver/0,switchedReceiver/0,switchedTimedReceiver/0,unswitchedReceiver/0,unswitchedTimedReceiver/0,orderedReceiver/0]).

receiver() -> % runs indefinitely
    timer:sleep(10000),
    receive
        X ->
            io:format("~w~n", [X])
    end,
    receiver().

switchedReceiver() ->
    receive
        X ->
            case X of
                stop -> 
                    io:format("~w~n", [X]);
                _ ->
                    io:format("~w~n", [X]),
                    switchedReceiver()
            end
    end.

switchedTimedReceiver() ->
    timer:sleep(10000),
    receive
        X ->
            case X of
                stop -> 
                    io:format("~w~n", [X]);
                _ ->
                    io:format("~w~n", [X]),
                    switchedTimedReceiver()
            end
    end.

unswitchedReceiver() ->
    receive
        stop ->
            io:format("~w~n", [stop]);
        X ->
            io:format("~w~n", [X]),
            unswitchedReceiver()
    end.

unswitchedTimedReceiver() ->
    timer:sleep(10000),
    receive
        stop ->
            io:format("~w~n", [stop]);
        X ->
            io:format("~w~n", [X]),
            unswitchedTimedReceiver()
    end.

orderedReceiver() ->
    receive
        {first, FirstString} ->
            io:format("~s~n", [FirstString]),
            receive
                {second, SecondString} ->
                    io:format("~s~n", [SecondString]),
                    orderedReceiver();
                stop ->
                    io:format("~s~n", [stop])
            end;
        stop ->
            io:format("~s~n", [stop])
    end.