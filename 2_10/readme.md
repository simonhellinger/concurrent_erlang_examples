### Frequency Server with supervisor ###

## Scope ##

## How to run ##

### Initial compile ###

c(frequency).
c(freqsuper).
c(simpleclient).

observer:start().

### Scenario 1: Frequency Server dies ###

freqsuper:start().
PidFs = freqsuper:startfs().
PidFs2 = freqsuper:startfs().
simpleclient:start(PidFs).
simpleclient:start(PidFs).
freqsuper:killfs(PidFs).

### Scenario 2: Supervisor dies ###

freqsuper:start().
PidFs = freqsuper:startfs().
PidFs2 = freqsuper:startfs().
simpleclient:start(PidFs).
simpleclient:start(PidFs).
freqsuper:stop().

### Scenario 3: Clients die ###

freqsuper:start().
PidFs = freqsuper:startfs().
PidFs2 = freqsuper:startfs().
simpleclient:start(PidFs).
simpleclient:start(PidFs).

% kill a simpleclient from observer. 