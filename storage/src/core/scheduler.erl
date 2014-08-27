%% @author Michal Liszcz
%% @doc scheduling module

-module(scheduler).
-include("shared.hrl").
-define(PROC, sched_proc).
-define(EXECUTOR_LIFESPAN, 180000).

-export([ init/0, deinit/0, execute/2 ]).

-ifdef(notrack).
-define(LOG_ACTION(X), true).
-else.
-define(LOG_ACTION(X), db_actions:store(X)).
-endif.

% ordsets are sorted from min to max

init() ->
	Executors = ets:new(executors_tab, [public]),
	JobQueue = ordsets:new(),
	register(?PROC, spawn(fun() -> main({Executors, JobQueue, 4}) end)).


deinit() ->
	?PROC ! stop.


execute(ReplyTo, #request{user = UserId, path = VPath}=Request) ->
	?LOG_INFO("pushing to scheduler"),
	?PROC ! {exec, {ReplyTo, Request}}.


main({Execs, Jobs, Slots} = Status) ->
	receive
		stop -> ets:delete(Execs), ok;

		{exec, {ReplyTo, #request{user = UserId, path = VPath}=Req} = _Msg} ->
			?LOG_INFO("scheduler got EXEC"),
			Priority = calc_prior(Req),
			Executor = get_executor(Execs, UserId++VPath),
			Executor ! {ReplyTo, Req},
			main(continue_jobs(
				{Execs, ordsets:add_element({Priority, Executor, make_ref()}, Jobs), Slots}
				));

		{done, _ExecName} ->
		?LOG_INFO("released"),
			main(continue_jobs(
				{Execs, Jobs, Slots+1}
				))
	end.

continue_jobs({Execs, Jobs, Slots} = Status)
	when Slots == 0; length(Jobs) == 0 ->
	?LOG_INFO("nothing to continue"),
	Status;

continue_jobs({Execs, [{_Prior, NextExec, Ref} | RestJobs], Slots} = Status) ->
	?LOG_INFO("continuing"),
	NextExec ! continue,
	continue_jobs({Execs, RestJobs, Slots-1}).


calc_prior(Req) -> 0.

%% executors

run(Execs, Name, SchedProc) ->
	receive
		{ReplyTo, #request{type=Type} = Request} ->

			receive
				continue -> pass
			end,

			case core:handle_req(Request) of
				{ok,	_}=Result -> gen_server:reply(ReplyTo, Result);
				{error,	_} -> pass
			end,

			?LOG_ACTION(Request),

			erlang:garbage_collect(self()),

			SchedProc ! {done, self()},

			run(Execs, Name, SchedProc)

	after ?EXECUTOR_LIFESPAN ->
		?LOG_INFO("inactive executor terminates itself"),
		ets:delete(Execs, Name)
	end.


%% @def retrieves (or creates) handler for given file
get_executor(Execs, Name) ->
	case ets:lookup(Execs, Name) of
		[{Name, ExecutorPid}] -> ExecutorPid;
		[] ->
			Me = self(),
			NewExecPid = spawn(fun() -> run(Execs, Name, Me) end),
			ets:insert(Execs, { Name, NewExecPid }),
			NewExecPid;
		_ -> {error, handler_not_found}
	end.
