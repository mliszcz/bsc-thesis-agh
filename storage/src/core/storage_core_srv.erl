%% @author Michal Liszcz
%% @doc Storage Server core module

-module(storage_core_srv).
-include("shared.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
% -define(EXEC_PROC, executor_proc).
-define(EXECUTORS, proc_executors).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	util:set_env(core_node_dir, filename:join([util:get_env(core_work_dir), atom_to_list(node())])),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->

	process_flag(trap_exit, true),	% this is left for the sake of example and will be removed

	% ets:new(?EXECUTORS, [named_table, public, {heir, whereis(init), nothing} ]),
	scheduler:init(),
	% register(single_executor, spawn(fun() -> executor:run("executor", 0) end)),

	Execs = [],
	% Execs = [
	% 	 spawn(fun() -> executor:run("executor1", 0) end),
	% 	 spawn(fun() -> executor:run("executor2", 0) end),
	% 	 spawn(fun() -> executor:run("executor3", 0) end),
	% 	 spawn(fun() -> executor:run("executor4", 0) end)
	% ],

	filelib:ensure_dir(files:resolve_name("files.db")),
	db_files:init(files:resolve_name("files.db")),

	filelib:ensure_dir(files:resolve_name("actions.db")),
	db_actions:init(files:resolve_name("actions.db")),

	Fill = case db_files:calculate_total_size() of
		Size when is_integer(Size) -> Size;
		null -> 0
	end,

	Quota = util:get_env(core_storage_quota),

	globals:set(fill, Fill),
	globals:set(reserv, 0),

	?LOG_INFO("node ~s, fill ~w/~w with ~w files",
		[node(), Fill, Quota, 0]),

	{ok, {Fill, Quota, {Execs, 1}}}.

handle_call({{request,
	#request{
		type=list
		}=Request}, _ReplyTo}, From, {_Fill, _Quota, _}=State) ->

	?LOG_INFO("received ~p from ~p (~p)", [Request#request.type, Request#request.user, Request#request.addr]),
	executor:push(From, Request),
	{noreply, State};

handle_call({reserve, HowMuch}, _From, State) ->
	?LOG_INFO("reserving ~p B", [HowMuch]),
	Fill = globals:get(fill),
	Reserv = globals:get(reserv),
	Quota = util:get_env(core_storage_quota),
	Result = case Fill+Reserv =< Quota of
		true ->
			globals:set(reserv, Reserv+HowMuch),
			{ok, (Fill+Reserv)/Quota};
		_	->
			{error, storage_full}
	end,
	?LOG_INFO("reserved"),
	{reply, Result, State}.

handle_cast({release, HowMuch}, State) ->
	?LOG_INFO("releasing ~p B", [HowMuch]),
	globals:set(reserv, globals:get(reserv)-HowMuch),
	{noreply, State};

% handle_cast({request,
% 	#request{
% 		type=find,
% 		path=Path,
% 		user=User
% 		}=_Request, ReplyTo}, {_Fill, _Quota}=State) ->

% 	case metadata:get(User, Path) of
% 		{ok, _} -> gen_server:reply(ReplyTo, {ok, node()});
% 		_ -> pass
% 	end,
% 	{noreply, State};

handle_cast({{request,
	#request{}=Request}, ReplyTo}, {_Fill, _Quota, {_Exec, _Next}}=State) ->

	?LOG_INFO("received ~p from ~p (~p)", [Request#request.type, Request#request.user, Request#request.addr]),

	%% TODO consider this
	% NewState = case {Type, metadata:get(User, Path)} of
	% 	{create,	{ok,	_}			} ->
	% 										?LOG_WARN("file exists!")
	% 										gen_server:reply(ReplyTo, {error, file_exists});
	% 	{create,	{error,	not_found}	} ->
	% 										executor:push(ReplyTo, Request),
	% 										{Fill+byte_size(Data), Quota};
	% 	{read,		{ok,	_}			} ->
	% 										executor:push(ReplyTo, Request),
	% 										{Fill, Quota}
	% end,

	% single_executor ! {ReplyTo, Request},
	% executor:push(ReplyTo, Request),

	scheduler:execute(ReplyTo, Request),

	% lists:nth(Next, Exec) ! {ReplyTo, Request},

	{noreply, State};
	% {noreply, {_Fill, _Quota, {Exec,(Next rem 4)+1}}};


handle_cast(stop, State) ->
	?LOG_INFO("shutdown"),
	% ets:delete(?EXECUTORS),
	scheduler:deinit(),
	db_files:deinit(),
	db_actions:deinit(),
	{stop, normal, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	?LOG_INFO("closing"),
	% ets:delete(?EXECUTORS),
	scheduler:deinit(),
	db_files:deinit(),
	db_actions:deinit(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
