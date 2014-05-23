%% @author Michal Liszcz
%% @doc Per-File core executors

-module(executor).
-include("shared.hrl").
-define(EXEC_PROC, executor_proc).
-define(EXECUTORS, proc_executors).

-export([ push/2 ]).

run() ->
	receive
		{ReplyTo, #rreq{} = Request} ->
			gen_server:reply(ReplyTo, core:handle_req(Request))
	end,
	run().

%% @def pushes request to associated executor and returns immediately
push(ReplyTo, #rreq{user_id = UserId, v_path = VPath}=Request) ->
	get_executor(UserId++VPath) ! {ReplyTo, Request},
	{ok, request_pushed}.

%% @def retrieves (or creates) handler for given file
get_executor(Name) ->
	case ets:lookup(?EXECUTORS, Name) of
		[{Name, ExecutorPid}] -> ExecutorPid;
		[] -> ets:insert(?EXECUTORS, { Name, spawn(fun run/0) }),
			  get_executor(Name);
		_ -> {error, handler_not_found}
	end.
