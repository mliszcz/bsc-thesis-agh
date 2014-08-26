%% @author Michal Liszcz
%% @doc scheduling module

-module(scheduler).
-include("shared.hrl").
-define(PROC, sched_proc).

-export([ init/0, deinit/0 ]).


init() ->
	Executors = ets.
	JobQueue = queue.
	register(?PROC, spawn(fun() -> main({Executors, JobQueue}) end)).


deinit() ->
	?PROC ! stop.


execute(ReplyTo, #request{user = UserId, path = VPath}=Request) ->
	?PROC ! {exec, {ReplyTo, Request}}.


main({Execs, Jobs} = Status) ->
	receive
		stop -> ok;
		{exec, {ReplyTo, #request{user = UserId, path = VPath}=Req} = _Msg} ->
			queue:in(Jobs, Req),
			get_executor(Execs, UserId++VPath) ! {ReplyTo, Req},
			main(Status);
		{done, ExecName} ->
			queue:out(Jobs),
			ExecName ! continue
			% enable next job or wait
			main(Status)
	end.



%% executors

run(Execs, Name, SchedProc) ->
	receive
		{ReplyTo, #request{type=Type} = Request} ->

			case core:handle_req(Request) of
				{ok,	_}=Result -> gen_server:reply(ReplyTo, Result);
				{error,	_} -> pass
			end,

			?LOG_ACTION(Request),

			erlang:garbage_collect(self()),

			SchedProc ! {done, Name},

			receive
				resume -> pass.
			end,

			run(Execs, Name, SchedProc)

	after ?EXECUTOR_LIFESPAN ->
		?LOG_INFO("inactive executor terminates itself"),
		ets:delete(Execs, Name)
	end.


%% @def retrieves (or creates) handler for given file
get_executor(Execs, Name) ->
	case ets:lookup(Execs, Name) of
		[{Name, ExecutorPid}] -> ExecutorPid;
		[] -> ets:insert(Execs, { Name, spawn(fun() -> run(Execs, Name, self()) end) }),
			  get_executor(Execs, Name);
		_ -> {error, handler_not_found}
	end.
