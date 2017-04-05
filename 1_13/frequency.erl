%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-include_lib("eunit/include/eunit.hrl").
-export([init/0,create_server/0]).

%% spawn and register the service and deliver that

create_server() ->
  register(frequency, spawn(frequency, init, [])).

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate(Frequencies = {[Freq|Free], Allocated}, Pid) ->
  ExistingPidFreq = get_allocated_frequency(Pid, Allocated),
  case ExistingPidFreq of
    {ok, no_frequency} ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
    _ ->
      {Frequencies, {error, frequency_allocated}}
  end.

deallocate(Frequencies = {Free, Allocated}, Freq, Pid) ->
  PidFreq = get_allocated_frequency(Pid, Allocated),
  case PidFreq of
    {ok, Freq} ->
      NewAllocated=lists:keydelete(Freq, 1, Allocated), 
      {{[Freq|Free], NewAllocated}, {ok, frequency_deallocated}};
    _ ->
      {Frequencies, {error, pid_not_frequency_holder}}
  end.
  
%% Returns the allocated frequency of a Pid, or no_frequency,
%% if the pid does not have one yet.
get_allocated_frequency(_Pid, []) ->
  {ok, no_frequency};
get_allocated_frequency(Pid, [{Freq, Pid} | _Allocated]) ->
  {ok, Freq};
get_allocated_frequency(Pid, [{_Freq, _OtherPid} | Allocated]) ->
  get_allocated_frequency(Pid, Allocated).


get_allocated_frequency_test() ->
  Pid = self(),
  ?assertEqual({ok, no_frequency}, get_allocated_frequency(Pid, [])),
  ?assertEqual({ok, 11}, get_allocated_frequency(Pid, [{11, Pid}])),
  ?assertEqual({ok, 11}, get_allocated_frequency(Pid, [{10, fakepid}, {11, Pid}, {12, fakepid}])),
  ?assertEqual({ok, no_frequency}, get_allocated_frequency(Pid, [{10, fakepid}, {11, fakepid}])).