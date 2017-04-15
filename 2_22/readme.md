# Hot-Swapping Code in Erlang

## Preparations

To hot-swap code, an erlang program must be susceptible to new code, and this works only if the functionality of a module is called from the outside. Functions calling other functions inside the module will not trigger code reload. Thus, for instance the loop must call itself via a public api:

```erl
-export([loop/1]).

loop(Frequencies) ->
    % ...
    frequency:loop(Frequencies);
    %...
```

Once this is done, we can compile code and swap it through triggering the loop

## Git for versioning

In this example, git holds the old and the new version of the program, and we'll use git commands to make first the original frequency server, and then the modified one, available on the erlang command line.

## Running the example

The following steps show the erlang commands, and the git command to load the right versions as comments. We assume that the original version of the frequency server (including the hot-swap modifications) is available as Git HEAD^, while the modified version is the latest node (master) in our tree.

```erl
% git checkout HEAD^
c(frequency).                   % {ok, frequency}

% start the server and allocate as many frequencies as we have
frequency:start().              % true
frequency:allocate().           % {ok, 10}
frequency:allocate().           % {ok, 11}
frequency:allocate().           % {ok, 12}
frequency:allocate().           % {ok, 13}
frequency:allocate().           % {ok, 14}
frequency:allocate().           % {ok, 15}
frequency:allocate().           % {error, no_frequency}

% to prove we're on the old code, let's try the new function and fail
frequency:inject([16,17,18]).   % ** exception error: undefined function frequency:inject/1

% git checkout master
c(frequency).                   % {ok, frequency}

% now that we have the new code compiled and loaded, we 
% need to make erlang exchange it. We do this by triggering
% a loop call.
frequency:allocate().           % {error, no_frequency}

% Now we have loaded our new code, let's use it
frequency:inject([16,17,18]).   % ok
frequency:allocate().           % {ok, 18}

% Things like code:load_file or code:soft_purge did not work at all as expected.
```