%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson


%% This is the provided frequency server with minor changes.
%% 1) The functional API is removed. All interaction is driven by
%%    the supervisor.
%% 2) The function start/0 is gone for the same reason
%% 3) I added a signal trap specifically for supervisor exits.

-module(frequency).
-export([init/1]).

init(SupervisorPid) ->
  process_flag(trap_exit, true),
  Frequencies = {get_frequencies(), []},
  loop(SupervisorPid, Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(SupervisorPid, Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
        Pid ! {reply, Reply},
        loop(SupervisorPid, NewFrequencies);
      {request, Pid , {deallocate, Freq}} ->
        NewFrequencies = deallocate(Frequencies, Freq),
        Pid ! {reply, ok},
        loop(SupervisorPid, NewFrequencies);
      {request, Pid, stop} ->
        Pid ! {reply, stopped};
      {'EXIT', SupervisorPid, _Reason} ->                               %% ADDED
        io:format("Supervisor ~w died, going down~n", [SupervisorPid]),
        exit(self(), kill);
      {'EXIT', Pid, _Reason} ->
        io:format("Client ~w died, ignoring~n", [Pid]),                 %% ADDED MESSAGE
        NewFrequencies = exited(Frequencies, Pid), 
        loop(SupervisorPid, NewFrequencies)
  end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),
  unlink(Pid),
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.

