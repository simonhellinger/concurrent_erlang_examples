-module(freqsuper).
-export([start/0, stop/0, startfs/0, killfs/1, stopfs/1]).
-export([init/0]).

%% The supervisor drives the frequency server lifecycle. Using the server should be easy.
%% Read the comments of every function in the API below to find out more.

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
        {request, Pid, {stopfs, FsPid}} ->
            Reply = stop_frequency_server(FsPid),
            Pid ! Reply,
            loop();
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        {'EXIT', Pid, normal} ->    %% TRAP REGULAR EXITS caused by stopfs()
            io:format("Frequency Server ~w shut down normally, ignoring~n", [Pid]),
            loop();
        {'EXIT', Pid, _Reason} ->   %% TRAP MORE VIOLENT DEATHS caused by killfs()
            io:format("Frequency Server ~w died, restarting~n", [Pid]),
            FsPid = create_new_frequency_server(),
            Pid ! {reply, FsPid},
            loop()
    end.


%%% Public API. All public api except start sends a message to 
%%% the message-loop and gets a message in return 

%% Starts the supervisor and registers it under freqsuper.
start() ->
    register(freqsuper,
        spawn(freqsuper, init, [])).

%% Stops the supervisor. All linked frequency servers will die along with it.
stop() ->
    freqsuper ! {request, self(), stop},
    receive
        {reply, Reply} ->
            Reply
    end.

%% Start a single frequency server and link it to the supervisor. Returns a Pid clients
%% can use, so you should probably capture this in a variable.
startfs() ->
    freqsuper ! {request, self(), startfs},
    receive
        {reply, Pid} ->
            Pid
    end.

%% Politely stops a frequency server identified by Pid. 
%% Clients will not notice this!
stopfs(Pid) ->
    freqsuper ! {request, self(), {stopfs, Pid}},
    receive
        Msg ->
            Msg
    end.

%% Kills the frequency server identified by Pid.
%% Clients die with it.
killfs(Pid) ->
    freqsuper ! {request, self(), {killfs, Pid}},
    receive
        Msg ->
            Msg
    end.


%%% Internal functionality
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