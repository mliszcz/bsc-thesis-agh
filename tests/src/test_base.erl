%% @author Michal Liszcz
%% @doc testing framework for storage server core (uses client api calls)

-module(test_base).
-define(VERBOSE, true).

-export([ test_create/0 ]).

%% ===================================================================
%% general runner
%% ===================================================================

% currently unused
% run(Threads, JobFun) ->
% 	run(Threads, 1, fun(_) -> JobFun() end).

run(Threads, Iterations, JobFun) ->
	SuperPid = self(),
	lists:foreach(fun(Num)-> spawn(fun() -> thread(SuperPid, Num, Iterations, JobFun) end) end, lists:seq(1, Threads)),
	lists:map(fun(_)-> receive Res -> Res end end, lists:seq(1, Threads)).

thread(Pid, ThreadNum, Iterations, JobFun) ->
	Pid ! iteration(ThreadNum, Iterations, JobFun, 0).

iteration(_ThreadNum, 0, _JobFun, TimeAcc) -> TimeAcc;
iteration( ThreadNum, N,  JobFun, TimeAcc) ->
	if ?VERBOSE -> io:format("thread ~p, iter ~p!~n", [ThreadNum, N]) end,
	{Microtime, _Value} = timer:tc(JobFun, [ThreadNum, N]),
	iteration(ThreadNum, N-1, JobFun, TimeAcc+Microtime).

% base test case
% SetupFun		- called with fixture in argument
% JobFun		- takes whatever the SetupFun returned (1st arg) thread num (2nd arg) and iter num (3rd arg)
% TeardownFun	- called with SetupFun return value
test_case(SetupFun, JobFun, TeardownFun) ->

	Fixture = fixture:setup(),
	Threads = fixture:config(threads, Fixture),
	Iterations = fixture:config(iterations, Fixture),
	Filesize = fixture:config(file_size, Fixture),

	Setup = SetupFun(Fixture),
	Times = run(Threads, Iterations, fun(T, N) -> JobFun(Setup, T, N) end),
	TeardownFun(Setup),

	fixture:teardown(Fixture),

	Result = {Filesize, Threads, Iterations, Times},
	file:write_file(fixture:config(logfile, Fixture), io_lib:fwrite("~p~n", [Result]), [append]),
	Result.


%% ===================================================================
%% tests
%% ===================================================================

test_create() ->

	SetupFun = fun(Fixture) ->
		{ok, Binary} = file:read_file(fixture:config(file_name, Fixture)),
		Node = fixture:config(gateway, Fixture),
		{list_to_atom(Node), Binary}
	end,

	TeardownFun = fun(_) -> pass end,

	ActionFun = fun({Node, Binary}, T, N) ->
		{ok, created} = storage:create(Node, integer_to_list(T)++"-"++integer_to_list(N), Binary)
	end,

	Result = test_case(SetupFun, ActionFun, TeardownFun),
	io:format("result is: ~p~n", [Result]).
