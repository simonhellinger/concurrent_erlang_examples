-module(router).
-include_lib("eunit/include/eunit.hrl").
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() ->
    register(router,
        spawn(router, init, [])).

init() ->
    Servers = start_frequency_servers(),
    Strategy = fun random_strategy/1,
    loop(Servers, Strategy).

%% main messaging loop
loop(Servers, Strategy) ->
    receive
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        {request, Pid, allocate} ->
            Pid ! {reply, allocate_frequency_on_server(Servers, Strategy)},
            loop(Servers, Strategy);
        {request, Pid, {deallocate, Freq}} ->
            Pid ! {reply, deallocate_frequency_on_server(Servers, Strategy, Freq)},
            loop(Servers, Strategy)
    end.

%% public functional api
allocate() ->
    router ! {request, self(), allocate},
    receive
        {reply, Reply} ->
            Reply
    end.

deallocate(Frequency) ->
    router ! {request, self(), {deallocate, Frequency}},
    receive
        {reply, Reply} ->
            Reply
    end.

stop() ->   % Linked servers will go down with the router
    router ! {request, self(), stop},
    receive
        {reply, Reply} ->
            Reply
    end.

%% private helper functions
start_frequency_servers() ->
    [start_frequency_server([10, 11, 12, 13,14, 15]), 
        start_frequency_server([20, 21, 22, 23, 24, 25])].

start_frequency_server(Frequencies) ->
    spawn_link(frequency, init, [Frequencies]).

allocate_frequency_on_server(Servers, Strategy) ->
    Server = Strategy(Servers),
    Server ! {request, self(), allocate},
    receive
        Reply ->
            Reply
    end.

deallocate_frequency_on_server(Servers, Strategy, Freq) ->
    Server = Strategy(Servers),
    Server ! {request, self(), {deallocate, Freq}},
    receive
        Reply ->
            Reply
    end.

random_strategy(Servers) ->
    Len = length(Servers),
    Pos = rand:uniform(Len),
    lists:nth(Pos, Servers).