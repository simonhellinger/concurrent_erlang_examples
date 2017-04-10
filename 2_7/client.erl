-module(client).
-export([start/0]).
-export([init/0]).

start() ->
    register(client,
        spawn(client, init, [])).
        
init() ->
    loop().

loop() ->
    allocateDeallocateFrequency(),
    loop().

allocateDeallocateFrequency() ->
    frequency ! {request, self(), allocate},
    receive
        {ok, Freq} ->
            deallocateFrequency(Freq)
    end.

deallocateFrequency(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive
        {reply, Reply} ->
            Reply
    end.