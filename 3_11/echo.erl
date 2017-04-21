-module(echo).
-behaviour(gen_server).
-export([start_link/0, stop/0, count/0, echo/1, reset/1]). % public api
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]). % gen_server callbacks

%%% public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

count() ->
    gen_server:call(?MODULE, count).

echo(Msg) ->
    gen_server:call(?MODULE, {echo, Msg}).

reset(X) ->
    gen_server:cast(?MODULE, {reset, X}).

%%% gen_server callbacks

% callback implementations

init([]) ->
    {ok, 0}.

handle_call(count, _From, State) ->
    {reply, State, State};
handle_call({echo, Msg}, _From, State) ->
    {reply, Msg, State + 1}.

handle_cast({reset, N}, _State) ->
    {noreply, N}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.