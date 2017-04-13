-module(simpleclient).
-export([start/1]).
-export([init/1]).

start(FrequencyServerPid) ->
    spawn(simpleclient, init, [FrequencyServerPid]).

init(FrequencyServerPid) ->
    FrequencyServerPid ! {request, self(), allocate},
    receive
        Msg ->
            Msg
    end,
    timer:sleep(infinity).