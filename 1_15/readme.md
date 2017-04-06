# Assignment 'Enhancing the Frequency Server'

## Scope

In this assignment, we were tasked to do four things to a pre-existing frequency server:

* Create a `clear/0` method with a timeout of 0, that flushes out all messages of a processes' mailbox and then shuts down.

* Add timeouts to client code (as I interpret it: the functional API). After the timeouts, the API should probably print a message that a timeout occurred.

* Add the clear methods in such a way that the process mailboxes are cleared out _periodically_.

* Make the server behave as if overloaded.

## Out of Scope

* To my eternal chagrin I failed at coming up with useful tests in this assignment. Testing spawned processes requires further knowledge that I do not yet possess.

## Running the example

After the usual compile, you can start the server either via

~~~erl
frequency:start().
~~~

or, if you'd like to simulate an overloaded server, with

~~~erl
frequency:start(3500).
~~~

which starts a server that adds a 3.5s sleep before returning any reply.

After that, you can just call the services the regular way via the functional API. If you chose to start the server with a large delay, you can now add a few calls to, say, `allocate/0` and see that no replies
come back at all.

~~~erl
frequency:allocate().
frequency:allocate().
frequency:allocate().
frequency:allocate().
~~~

Only once the example's 3.5s have passed will any next call show you that the mailbox is emptied via `clear_verbose/0`.

## Solution

### `clear/0`

Supplying the clear/0 function with a timeout of 0 basically meant looking up the solution presented in one of the slides in the videos. I added a function `clear_verbose/0`, that prints a message whenever the inbox is cleared out.

### The timeouts

I added a fixed timeout of 500ms to each `receive/0` statement of the functional API, so `allocate/0`, `deallocate/1` and `stop/0`.

### Periodic cleaning out of mailboxes

I assumed that the task was to simulate the long response times of a server, so the clients are the ones waiting for messages. Thus it is the clients eventually timing out, printing out messages.

For this reason I added the `clear_verbose/0` call to each API call, before the message is sent on to the server. I think this is a good position to clear out old messages. It is by far not a good solution as the content of the messages should probably trigger some client behaviour, but this, I believe, is a task for later assignments.

### The overloaded server

In order to make the server behave as if overloaded, I added a call to `timer:sleep/1` directly before sending any response. This should do the trick in our limited environment.

I extended the functionality of function `start/0` with a separate function `start/1` that takes the forced delay in milliseconds. The function `start/0` just calls `start/1` with a delay of 0.

This way you can choose which one server to run.

## What I learned

The thing I took away from this assignment is that you can use the `receive ... end` statement in any process, and if you don't explicitly spawn a new process, it uses the one that's already there. As each `receive` statement goes to the mailbox of its process, you can access the same mailbox via multiple functions having a `receive` block.

So multiple `receive` blocks created in the scope of one process subscribe as kind of listeners to a shared process-mailbox, working off messages (by default, at least) in a stack-like manner.

That wasn't at all clear to me in the beginning, which is why I didn't understand how the `clear/0` function was supposed to access old mails in the mailbox.