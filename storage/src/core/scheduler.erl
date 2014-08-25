%% @author Michal Liszcz
%% @doc scheduling module

-module(scheduler).
-include("shared.hrl").
-define(PROC, sched_proc).

-export([ init/0, deinit/0 ]).


init() ->
	register(?PROC, spawn(fun() -> main([]) end)).


deinit() ->
	?PROC ! stop.


execute(ReplyTo, #request{user = UserId, path = VPath}=Request) ->
	?PROC ! {exec, {ReplyTo, Request}}.


main(Status) ->
	receive
		stop -> ok;
		{exec, {RT, Req} = _Msg} ->
			executor:push(RT, Req),
			main(Status)
	end.
