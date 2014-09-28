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


execute(ReplyTo, #request{}=Request) ->
	?LOG_INFO("pushing to scheduler"),
	?PROC ! {exec, {ReplyTo, Request}}.


main({Execs, Jobs, Slots} = Status) ->
	receive
		stop -> ets:delete(Execs), ok;

		{exec, {ReplyTo, #request{user = IssuerId, addr = {UserId, VPath}}=Req} = _Msg} ->
			?LOG_INFO("scheduler got EXEC"),
			Priority = -calc_prior(Req),
			Executor = get_executor(Execs, UserId++VPath),
			Executor ! {ReplyTo, Req},
			main(continue_jobs(
				{Execs, add_element({Priority, Executor, make_ref()}, Jobs), Slots}
				));

		{done, _ExecName} ->
		?LOG_INFO("released"),
			main(continue_jobs(
				{Execs, Jobs, Slots+1}
				))
	end.


add_element(E, S) -> ordsets_add(E, [], S).

% last item has highest priority
ordsets_add(E, L, []) ->
	lists:append(L, [E]);
ordsets_add({P, X, _} = E, L, [{P0, X0, _}=R0|R]) ->
	case {X == X0, P > P0} of
		{false, true} ->ordsets_add(E, lists:append(L, [R0]), R);
		_ -> lists:append([L, [E,R0], R])
	end.


continue_jobs({Execs, Jobs, Slots} = Status)
	when Slots == 0; length(Jobs) == 0 ->
	?LOG_INFO("nothing to continue"),
	Status;

continue_jobs({Execs, Jobs, Slots} = Status) ->
	?LOG_INFO("continuing"),
	{_Prior, NextExec, _Ref} = lists:last(Jobs),
	NextExec ! continue,
	continue_jobs({Execs, lists:droplast(Jobs), Slots-1}).


calc_prior(#request{type=Type}) ->
	case Type of
		find -> 0;
		read -> 1;
		create -> 2;
		update -> 3;
		_ -> 4
	end.

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
