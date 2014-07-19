
-module(test_base).
-define(VERBOSE, true).

-export([ test_create/4 ]).

% load_config() ->
% 	pass.

%% ===================================================================
%% general runner
%% ===================================================================

run(Threads, Iterations, JobFun) ->
	SuperPid = self(),
	lists:foreach(fun(_)-> spawn(fun() -> thread(SuperPid, Iterations, JobFun) end) end, lists:seq(1, Threads)),
	lists:map(fun(_)-> receive Res -> Res end end, lists:seq(1, Threads)).

thread(Pid, Iterations, JobFun) ->
	Pid ! iteration(Iterations, JobFun, 0).

run(Threads, JobFun) ->
	run(Threads, 1, fun(_) -> JobFun() end).

iteration(0, _JobFun, TimeAcc) -> TimeAcc;
iteration(N, JobFun,  TimeAcc) ->
	if ?VERBOSE -> io:format("thread ~p iter ~p!~n", [self(), N]) end,
	{Microtime, _Value} = timer:tc(JobFun, [N]),
	iteration(N-1, JobFun, TimeAcc+Microtime).


%% ===================================================================
%% tests
%% ===================================================================

test_create(Node, Threads, Iterations, FilePath) ->
	{ok, Binary} = file:read_file(FilePath),
	RootPath = "performance/tests/file",
	Result = run(Threads, Iterations, fun(N)->
		{ok, created} = storage:create(Node, RootPath++integer_to_list(N), Binary)
	end),

	io:format("result is: ~p~n", [Result]).
