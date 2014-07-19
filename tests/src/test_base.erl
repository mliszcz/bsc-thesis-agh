
-module(test_base).
-define(VERBOSE, true).

% -export([ test_create/0 ]).

-compile(export_all).

%% ===================================================================
%% general runner
%% ===================================================================

% currently unused
% run(Threads, JobFun) ->
% 	run(Threads, 1, fun(_) -> JobFun() end).

run(Threads, Iterations, JobFun) ->
	SuperPid = self(),
	lists:foreach(fun(_)-> spawn(fun() -> thread(SuperPid, Iterations, JobFun) end) end, lists:seq(1, Threads)),
	lists:map(fun(_)-> receive Res -> Res end end, lists:seq(1, Threads)).

thread(Pid, Iterations, JobFun) ->
	Pid ! iteration(Iterations, JobFun, 0).

iteration(0, _JobFun, TimeAcc) -> TimeAcc;
iteration(N, JobFun,  TimeAcc) ->
	if ?VERBOSE -> io:format("thread ~p iter ~p!~n", [self(), N]) end,
	{Microtime, _Value} = timer:tc(JobFun, [N]),
	iteration(N-1, JobFun, TimeAcc+Microtime).

% base test case
% SetupFun		- called with fixture in argument
% JobFun		- takes whatever the SetupFun returned (1st arg) and iteration number (2nd arg)
% TeardownFun	- called with SetupFun return value
test_case(SetupFun, JobFun, TeardownFun) ->
	Fixture = fixture:setup(),
	Setup = SetupFun(Fixture),
	Result = run(fixture:config(threads, Fixture), fixture:config(iterations, Fixture),
		fun(N) -> JobFun(Setup, N) end),
	TeardownFun(Setup),
	fixture:teardown(Fixture),
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

	ActionFun = fun({Node, Binary}, N) ->
		{ok, created} = storage:create(Node, integer_to_list(N), Binary)
	end,

	Result = test_case(SetupFun, ActionFun, TeardownFun),

	% Result = run(fixture:config(threads), fixture:config(iterations), fun(N)->
	% 	{ok, created} = storage:create(Node, integer_to_list(N), Binary)
	% end),

	% fixture:teardown(Fixture),

	io:format("result is: ~p~n", [Result]).
