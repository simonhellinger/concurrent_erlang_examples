-module(palin).
-include_lib("eunit/include/eunit.hrl").
-export([palin/1,nopunct/1,palindrome/1,server/1,multiserver/0,client/1,load_balancer/1]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

% Single client server
server(Pid) ->
    receive
        {check, Msg} ->
            Pid ! {result, quote(Msg) ++ palindrome_message(palindrome(Msg))},
            server(Pid);
        _ ->
            io:format("Server stopped~n")
    end.

% Multi client server
multiserver() ->
    receive
        {Pid, check, Msg} ->
            io:format("~p received message ~s from ~p~n", [self(), Msg, Pid]),
            Pid ! {result, quote(Msg) ++ palindrome_message(palindrome(Msg))},
            multiserver();
        _ ->
            io:format("Server ~p stopped~n", [self()])
    end.

% Load balancer with random pick strategy
load_balancer(Servers) ->
    receive
        P = {_Pid, check, _Msg} ->
            S = pick_server(fun random_strategy/1, Servers),
            S ! P,
            load_balancer(Servers);
        _ -> 
            io:format("Load balancer stopped~n")
    end.

pick_server(Strategy, Servers) ->
    Strategy(Servers).

random_strategy(Servers) ->
    N = rand:uniform(length(Servers)),
    lists:nth(N, Servers).

client(Server) ->
    Self = self(),
    Server ! {Self, check, "Madam, I'm Adam"},
    receive
        {result, Msg} -> 
            io:format("received ~s~n", [Msg]);
        _ ->
            io:format("Got bull.")
    end.


quote(Msg) ->
    "\"" ++ Msg ++ "\"".

palindrome_message(true) ->
    " is a palindrome.";
palindrome_message(false) ->
    " is not a palindrome.".

quote_test() ->
    ?assertEqual("\"\"", quote("")),
    ?assertEqual("\"Blah\"", quote("Blah")).