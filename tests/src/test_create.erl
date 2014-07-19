
-module(test_create).

-export([ run/0 ]).

run() ->
	test_base:run(3, fun test/0).

test() ->
	iteration(5),
	io:format("thread ~p done!~n", [self()]).

iteration(0) -> pass;
iteration(N) ->
	io:format("thread ~p iter ~p!~n", [self(), N]),
	timer:sleep(1000),
	iteration(N-1).
