%% @author Michal Liszcz
%% @doc Storage Server core module

-module(storage_core_srv).
-include("shared.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(EXEC_PROC, executor_proc).
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

init(Args) ->
	old_init(),
	{ok, Args}.

%% ------------------------------------------------------------------
%% new-style calls
%% ------------------------------------------------------------------

handle_call(_Request, From, State) ->
	info:error("unsupported call received from ~p", [From]),
	{reply, ok, State}.

%% ------------------------------------------------------------------
%% new-style casts
%% ------------------------------------------------------------------

handle_cast({request, #request{path=Path}=Request, ReplyTo}, State) ->
	log:info("requested ~s", [Path]),
	executor:push(ReplyTo, Request),
	{noreply, State};

handle_cast(stop, State) ->
	old_deinit(),
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

old_init() ->

	filelib:ensure_dir(files:resolve_name(".metadata")),
	metadata:init(files:resolve_name(".metadata")),
	
	%% calculate fill
	Files = metadata:to_list(),
	Fill = lists:foldl(fun(File, Acc) ->
							   Acc + File#filedesc.size
					   end, 0, Files),

	globals:set(fill, Fill),
	
	% register(?EXEC_PROC, spawn_link(fun executor/0)),
	ets:new(?EXECUTORS, [named_table, public, {heir, whereis(init), nothing} ]),

	log:info("node ~s, fill ~w/~w with ~w files",
		[node(), globals:get(fill), globals:get(capacity), length(Files)]).

old_deinit() ->
	io:format("~w: bye. luv ja.~n", [erlang:localtime()]),
	ets:delete(?EXECUTORS),
	metadata:deinit().
