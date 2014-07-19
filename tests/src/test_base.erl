
-module(test_base).

-export([ run/2 ]).

% load_config() ->
% 	pass.

run(Threads, JobFun) ->
	SuperPid = self(),
	lists:foreach(fun(_)-> spawn(fun() -> thread(SuperPid, JobFun) end) end, lists:seq(1, Threads)),
	lists:map(fun(_)-> receive Res -> Res end end, lists:seq(1, Threads)).

thread(Pid, JobFun) ->
	{Microtime, _Value} = timer:tc(JobFun),
	Pid ! Microtime.
