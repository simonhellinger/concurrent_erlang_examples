-module(freqsuper).
-export([start/0]).
-export([init/0]).


start() ->
    register(freqsuper, 
        spawn(freqsuper, init, [])).

init() ->
    process_flag(trap_exit, true),
    start_frequency_server(),
    loop().

loop() ->
    timer:sleep(5000),
    io:format("Next loop~n"),
    loop().

start_frequency_server() ->
    spawn_link(frequency, init, []).