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
	log:info("starting"), 
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->

	ets:new(?EXECUTORS, [named_table, public, {heir, whereis(init), nothing} ]),

	filelib:ensure_dir(files:resolve_name(".metadata")),
	metadata:init(files:resolve_name(".metadata")),

	Files = metadata:to_list(),
	Fill = lists:foldl(
		fun(File, Acc) -> Acc+File#filedesc.size end, 0, Files),

	Quota = util:get_env(core_storage_quota),

	globals:set(fill, Fill),
	globals:set(reserv, 0),

	log:info("node ~s, fill ~w/~w with ~w files",
		[node(), Fill, Quota, length(Files)]),

	{ok, {Fill, Quota}}.

handle_call({request,
	#request{
		type=list
		}=Request, _ReplyTo}, From, {_Fill, _Quota}=State) ->

	log:info("core list"),
	executor:push(From, Request),
	{noreply, State};

handle_call({reserve, HowMuch}, _From, State) ->
	log:info("reserving ~p B", [HowMuch]),
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
	{reply, Result, State}.

handle_cast({release, HowMuch}, State) ->
	log:info("releasing ~p B", [HowMuch]),
	globals:set(reserv, globals:get(reserv)-HowMuch),
	{noreply, State};

handle_cast({request,
	#request{
		type=_Type,
		path=Path,
		user=_User,
		data=_Data
		}=Request, ReplyTo}, {_Fill, _Quota}=State) ->

	log:info("requested ~s", [Path]),

	%% TODO consider this
	% NewState = case {Type, metadata:get(User, Path)} of
	% 	{create,	{ok,	_}			} ->
	% 										log:warn("file exists!")
	% 										gen_server:reply(ReplyTo, {error, file_exists});
	% 	{create,	{error,	not_found}	} ->
	% 										executor:push(ReplyTo, Request),
	% 										{Fill+byte_size(Data), Quota};
	% 	{read,		{ok,	_}			} ->
	% 										executor:push(ReplyTo, Request),
	% 										{Fill, Quota}
	% end,

	executor:push(ReplyTo, Request),

	{noreply, State};

handle_cast(stop, State) ->
	io:format("shutdown"),
	ets:delete(?EXECUTORS),
	metadata:deinit(),
	{stop, normal, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
