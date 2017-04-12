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
    loop(undefined).

loop(Freq) ->
    case whereis(frequency) of
        undefined ->
            io:format("~w: Server unavailable, sleeping for 5s.~n", [self()]),
            timer:sleep(5000),
            loop(undefined);
        _ ->
            Result = case Freq of
                undefined ->
                    allocate();
                _ ->
                    deallocate(Freq),
                    undefined
            end,
            timer:sleep(5000),
            loop(Result)
    end.

allocate() ->
    io:format("~w: allocating frequency~n", [self()]),
    frequency ! {request, self(), allocate},
    receive
        {reply, {ok, Freq}} ->
            Freq;
        {'EXIT', _Pid, _Reason} -> % not processed at the right time, what's wrong?
            io:format("uh oh~n"),
            undefined
    end.

deallocate(Freq) ->
    io:format("~w: deallocating frequency ~w~n", [self(), Freq]),
    frequency ! {request, self(), {deallocate, Freq}},
    receive
        {reply, _Reply} ->
            ok;
        {'EXIT', _Pid, _Reason} ->
            io:format("uh oh~n")
    end.

kill(ProcessId) ->
    exit(whereis(ProcessId), kill).