-module(foo).
-export([bar/0, bar/1, baz/0, bazz/0]).

% supplied by teacher

bar() ->
    timer:sleep(500),
    format:io("bar started~n"),
    format:io("bar working~n"),
    format:io("bar finished~n").

bar(Pid) ->
    Pid ! "bar started~n",
    Pid ! "bar working~n",
    Pid ! "bar finished~n".

baz() -> % runs indefinitely
    receive
        Msg ->
            io:format("got: ~s~n", [Msg])
    end,
    baz().

bazz() -> % better, stops on stop
    receive
        stop ->
            io:format("stopped~n");
        Msg ->
            io:format("got ~s~n", [Msg]),
            bazz()
    end.
    