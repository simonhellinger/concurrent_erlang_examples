# Frequency Server with supervisor and clients #

## Scope ##

In this task I tried to create a supervisor (freqsuper.erl) managing an arbitrary number of frequency servers (frequency.erl) that, in turn, can each have an arbitrary number of clients (simpleclient.erl). The focus clearly lies on the supervisor logic, and on the behavior of servers and clients whenever one of the participants dies.

The concessions I made to achieve this are:

* I removed the functional API from a preexisting frequency.erl, as it is not needed. I did add another Message filter tuned explicitly to catch supervisor EXIT signals.

* I created a simple client that allocates a frequency and then goes to sleep for eternity.

* The supervisor follows only a single restart strategy, namely restarting the server that just died. This is due to lack of time.

## How to run ##

The following is a list of commands to see the different scenarios in action. I did add output messages to the relevant sections so you can see who's reacting to what signal, but most important information can be gained via the observer.

### Initial compile ###

First, get the system ready:

~~~erl
c(frequency).
c(freqsuper).
c(simpleclient).

observer:start().
~~~

The following assumes that the supervisor `freqsuper` is the driving force behind all interaction. Therefore we use mostly the supervisors api.

### Scenario 1: Frequency Server dies ###

~~~erl
% Start supervisor
freqsuper:start().

% Start two frequency servers
PidFs = freqsuper:startfs().
PidFs2 = freqsuper:startfs().

% Link clients with the first frequency server
simpleclient:start(PidFs).
simpleclient:start(PidFs).

% Kill a single frequency server and see it's clients vanish.
% A new frequency server appears.
freqsuper:killfs(PidFs).
~~~

### Scenario 2: Supervisor dies ###

~~~erl
% Start supervisor
freqsuper:start().

% Start two frequency servers
PidFs = freqsuper:startfs().
PidFs2 = freqsuper:startfs().

% Link clients with the first frequency server
simpleclient:start(PidFs).
simpleclient:start(PidFs).

% Kill everything in your wake.
freqsuper:stop().
~~~

### Scenario 3: Clients die ###

~~~erl
% Start supervisor
freqsuper:start().

% Start two frequency servers
PidFs = freqsuper:startfs().
PidFs2 = freqsuper:startfs().

% Link clients with the first frequency server
simpleclient:start(PidFs).
simpleclient:start(PidFs).

% kill a simpleclient from observer console. Observer that all the other
% services keep running.
~~~