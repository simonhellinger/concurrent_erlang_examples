# From loop to gen_server

## The task

The task here was to take a non-generic server (frequency2.erl) and create a new one off it, using the gen_server behaviour.

## gen_server

The `gen_server` behaviour expects us to fill in a minimum list of callbacks. The only two relevant ones are `handle_call` and `handle_cast`.

* init
* handle_call ... all synchronous logic resides here
* handle_cast ... all asynchronous logic goes here
* handle_info
* terminate
* code_change

## Provided files

* gf.erl is the template to type in
* frequency2.erl is the supporting file
* frequency_gen_server.erl is the solution provided by the kent computing team, to compare your solution with.

## Extras

There's an echo server implementation here as well, which I did mainly for practicing reasons.

