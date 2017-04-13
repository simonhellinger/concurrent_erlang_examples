-module(freqsuper).
-export([start/0, stop/0, startfs/0, killfs/1, stopfs/1]).
-export([init/0]).

init() ->
    process_flag(trap_exit, true), % trap exits of all our frequency servers
    loop().

loop() ->
    receive
        {request, Pid, startfs} ->
            FsPid = create_new_frequency_server(),
            Pid ! {reply, FsPid},
            loop();
        {request, Pid, {killfs, FsPid}} ->
            kill_frequency_server(FsPid),
            Pid ! {reply, force_stopped},
            loop();
        {request, Pid, {stop, FsPid}} ->
            Reply = stop_frequency_server(FsPid),
            Pid ! Reply,
            loop();
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        {'EXIT', Pid, _Reason} ->
            io:format("Frequency Server ~w died, restarting~n", [Pid]),
            FsPid = create_new_frequency_server(),
            Pid ! {reply, FsPid},
            loop()
    end.


%%% Public API. All public api except start sends a message to the message-loop and gets a message in return 

start() ->
    register(freqsuper,
        spawn(freqsuper, init, [])).

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
    freqsuper ! {request, self(), {killfs, Pid}},
    receive
        Msg ->
            Msg
    end.

killfs(Pid) ->
    freqsuper ! {request, self(), {killfs, Pid}},
    receive
        Msg ->
            Msg
    end.

%%% Frequency Server handling
create_new_frequency_server() ->
    spawn_link(frequency, init, [self()]).

kill_frequency_server(FsPid) ->
    exit(FsPid, kill).

stop_frequency_server(FsPid) ->
    FsPid ! {request, self(), stop},
    receive
        {reply, Reply} ->
            Reply
    end.

    