-module(freqsuper).
-export([start/0, stop/0, startfs/0, stopfs/1]).
-export([init/0]).

init() ->
    process_flag(trap_exit, true), % trap exits of all our frequency servers
    loop([]).

loop(FreqServers) ->
    receive
        {request, Pid, startfs} ->
            FsPid = spawn_link(frequency, init, []),
            Pid ! {reply, FsPid},
            loop([FsPid|FreqServers]);
        {request, Pid, {stopfs, FsPid}} ->
            FsPid ! {request, Pid, stop},
            loop(removePidFromList(Pid, FreqServers));
        {request, Pid, stop} ->
            Pid ! {reply, ok};
        {'EXIT', Pid, normal} ->
            loop(removePidFromList(Pid, FreqServers));
        {'EXIT', Pid, shutdown} ->
            loop(removePidFromList(Pid, FreqServers));
        {'EXIT', Pid, _Reason} ->
            FsPid = spawn_link(frequency, init, []),
            Pid ! {reply, FsPid},
            loop([FsPid|FreqServers])
    end.


%%% Public API

start() ->
    register(freqsuper, spawn(freqsuper, init, [])).

stop() ->
    freqsuper ! {request, self(), stop},
    receive
        {reply, Reply} ->
            Reply
    end.

startfs() ->
    freqsuper ! {request, self(), startfs},
    receive
        {reply, Pid} ->
            Pid
    end.
    
stopfs(Pid) ->
    freqsuper ! {request, self(), {stopfs, Pid}},
    receive
        Msg ->
            Msg
    end.

%%% Internal helper function

% Manage the list of living FrequencyServers

removePidFromList(Pid, Pids) ->
    removePidFromList(Pid, Pids, []).

removePidFromList(_Pid, [], Result) ->
    Result;
removePidFromList(Pid, [Pid|Pids], Result) ->
    removePidFromList(Pid, Pids, Result);
removePidFromList(Pid, [OtherPid|Pids], Result) ->
    removePidFromList(Pid, Pids, [OtherPid|Result]).
