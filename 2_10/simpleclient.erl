-module(simpleclient).
-export([start/1]).
-export([init/1]).

%%% This client does nothing much. Tries to allocate a new frequency
%%% and goes to sleep forever. Useful only for observing.

start(FrequencyServerPid) ->
    spawn(simpleclient, init, [FrequencyServerPid]).

init(FrequencyServerPid) ->
    FrequencyServerPid ! {request, self(), allocate},
    receive
        Msg ->
            Msg
    end,
    timer:sleep(infinity).