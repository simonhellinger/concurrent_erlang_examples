%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson


%% Usage:
%% To start a server that answers quickly, issue
%% in the console:
%% 
%%    frequency:start(). 
%%
%% For a server that delays 3500ms 
%% before each answer, type this into the console:
%%
%%    frequency:start(3500).
%%
%% With a client receive timeout of a fixed 500ms, a few calls
%% will time out before the first response comes back.
%%
%% Running frequency:allocate() a few times in quick succession
%% will not produce any replies from the server until it's
%% too late. When the 3500ms have passed, call frequency:allocate()
%% again and you'll see the clear function throw out
%% a series of allocation replies.
%%
%% I run clear_verbose/0 as first statement in each
%% API call, thus missed responses will remain in the mailbox until
%% the next API call. Short of actually creating a timed recursive
%% function that cleans messages out periodically in the true sense
%% of the word, I think that's a good trade-off.

-module(frequency).
-export([start/0,start/1,allocate/0,deallocate/1,stop/0]). % API calls
-export([init/0,init/1]). % needed by API call 'start'

%% These are the start functions used to create and
%% initialize the server.

% starts a regular server with no delays

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  init(0).

% starts a slow server that delays every reply by DelayInMs

start(DelayInMs) ->
    register(frequency,
	     spawn(frequency, init, [DelayInMs])).

init(DelayInMs) ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies, DelayInMs).

% Hard Coded

get_frequencies() -> [10,11,12,13,14,15].
get_receive_timeout_ms() -> 500.

%% The Main Loop

loop(Frequencies, DelayInMs) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      timer:sleep(DelayInMs),
      Pid ! {reply, Reply},
      loop(NewFrequencies, DelayInMs);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      timer:sleep(DelayInMs),
      Pid ! {reply, ok},
      loop(NewFrequencies, DelayInMs);
    {request, Pid, stop} ->
      timer:sleep(DelayInMs),
      Pid ! {reply, stopped}
  end.

%% Functional interface

% Each functional interface cleans out old messages before
% issuing a new command.

allocate() ->
  clear_verbose(),
  frequency ! {request, self(), allocate},
  receive 
    {reply, Reply} -> 
      Reply
    after get_receive_timeout_ms() ->
      {error, timeout}
  end.

deallocate(Freq) ->
  clear_verbose(),
  frequency ! {request, self(), {deallocate, Freq}},
  receive 
    {reply, Reply} -> 
      Reply
    after get_receive_timeout_ms() ->
      {error, timeout}
  end.

stop() ->
  clear_verbose(),
  frequency ! {request, self(), stop},
  receive 
    {reply, Reply} -> 
      Reply
    after get_receive_timeout_ms() ->
      {error, timeout}
  end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

%% The clear function removes all 
%% messages from the mailbox and then exits.

%clear() ->
%	receive
%		_Msg ->
% 		clear()
%		after 0 ->
%			ok
%	end.

%% The verbose clear function prints and removes all 
%% messages from the mailbox and then exits.

clear_verbose() ->
  receive
		Msg ->
      io:format("Throwing out message ~w~n", [Msg]),
			clear_verbose()
		after 0 ->
			ok
	end.
