-module(client).
-export([start/1, kill/1]).
-export([init/0]).

start(Clientname) ->
    register(Clientname,
        spawn(client, init, [])).
        
init() ->
    % prevent getting killed when frequency server dies 
    % (clients are linked by frequency server).
    process_flag(trap_exit, true),
    loop().

loop() ->
    case whereis(frequency) of
        undefined ->
            io:format("Server unavailable, sleeping~n"),
            timer:sleep(5000);
        _ ->
            io:format("Server available, doing stuff~n"),
            timer:sleep(5000)
    end,
    loop().

allocate() ->
    frequency ! {request, self(), allocate},
    receive
        {reply, {ok, Freq}} ->
            Freq;
        {'EXIT', _Pid, _Reason} ->
            io:format("uh oh~n")
    end.

deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive
        {reply, _Reply} ->
            ok;
        {'EXIT', _Pid, _Reason} ->
            io:format("uh oh~n")
    end.

kill(ProcessId) ->
    exit(whereis(ProcessId), kill).