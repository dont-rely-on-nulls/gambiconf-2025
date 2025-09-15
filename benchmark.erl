-module(benchmark).

-export([loop/2, start/1]).

loop(Pid, 0) ->
    Pid ! ok;
loop(Pid, N) ->
    NewPid = spawn(?MODULE, ?FUNCTION_NAME, [Pid, N - 1]),
    NewPid ! ok,
    receive
        ok ->
            ok
    end.

start([A]) ->
    L = erlang:atom_to_list(A),
    N = erlang:list_to_integer(L),
    {Milis, ok} = timer:tc(?MODULE, loop, [self(), N], millisecond),
    io:format("Creating ~p processes took ~p milliseconds~n", [N, Milis]).
