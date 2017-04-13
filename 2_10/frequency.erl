%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/1]).


init(SupervisorPid) ->
  process_flag(trap_exit, true),    %%% ADDED
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
      {'EXIT', SupervisorPid, _Reason} ->
        io:format("Supervisor ~w died, going down~n", [SupervisorPid]),
        exit(self(), kill);
      {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
        io:format("Client ~w died, ignoring~n", [Pid]),
        NewFrequencies = exited(Frequencies, Pid), 
        loop(SupervisorPid, NewFrequencies)
  end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.

