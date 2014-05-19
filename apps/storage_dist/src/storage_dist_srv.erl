%% @author Michal Liszcz
%% @doc Storage Server communication module

-module(storage_dist_srv).
-include("shared.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, test/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	log:info("DIST: starting~n"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).

test() ->
	gen_server:call(?SERVER, {request, #rreq{action = get, v_path = "/empty", user_id="user1"}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->

	globals:set(capacity, util:get_env(dist_storage_cap)),

	InitialNode = list_to_atom(util:get_env(dist_initial_node)),
	RemoteNodes = remote_scan(sets:add_element(InitialNode, sets:new())),
	say_hello(RemoteNodes),

	{ok, RemoteNodes}.

handle_call({request, #rreq{action=get, v_path=Path}=Request}, From, State) ->
	info:log("DIST: broadcasting GET '~s'~n", [Path]),
	broadcast(State, ?CORE_SERVER, {request, Request, From}),
	{noreply, State};

handle_call({request, #rreq{action=put}=Request}, From, State) ->
	log:info("DIST: PUT requested~n"),
	[T|_]=sets:to_list(State),
	gen_server:cast({?CORE_SERVER, T}, {request, Request, From}),
	{noreply, State};

handle_call({request, #rreq{action=list, user_id=UserId}}=Request, From, State) ->
	log:info("DIST: list requested~n"),
	spawn_link(
		fun() ->
			List = broadcall_list(State, ?CORE_SERVER, {request, Request}),
			log:info("DIST: list served: ~p~n", [List]),
			gen_server:reply(From, {ok, List})
		end),
	{noreply, State};

handle_call({remotes}, _From, State) ->
	{reply, {ok, State}, State};

handle_call({request_storage, RequiredCap}, _From, State) ->
	case RequiredCap =< (globals:get(capacity)-globals:get(fill)) of
		true -> {reply, {ok, globals:get(fill)/globals:get(capacity)}, State};
		_ -> {reply, {error, storage_full}, State}
	end;

handle_call({reserve_storage, RequiredCap}, _From, State) ->
	log:info("DIST: reserving...~n"),
	globals:set(fill, globals:get(fill)+RequiredCap),
	log:info("DIST: reserved!~n"),
	{reply, {ok, reserved}, State}.

handle_cast(#request{} = Request, State) ->
	% handle_request(Request, none),
	{noreply, State};

handle_cast({hello, Node}, State) ->
	{noreply, sets:add_element(Node, State)};

handle_cast(stop, State) ->
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

%% @doc Scans all known remote nodes to find about new ones
% @FIXME catch and filter offline nodes here
remote_scan(RemoteNodes) ->
	sets:fold(
		fun(Node, Acc) ->
			try gen_server:call({?DIST_SERVER, Node}, {remotes}) of
				{ok, NodeSet} -> sets:union(Acc, NodeSet)
			catch
				_:_ -> Acc
			end
		end,
		RemoteNodes, RemoteNodes).

%% @doc Broadcasts own name to all nodes in the system
say_hello(RemoteNodes) ->
	broadcast(RemoteNodes, ?DIST_SERVER, {hello, node()}).

broadcast(RemoteNodes, Process, Message) ->
	sets:fold(
		fun(Node, _Acc) ->
			gen_server:cast({Process, Node}, Message)
		end,
		[], RemoteNodes).

broadcall_list(RemoteNodes, Process, Message) ->
	sets:fold(
		fun(Node, Acc) ->
			{ok, Res} = gen_server:call({Process, Node}, Message),
			Acc++Res
		end,
		[], RemoteNodes).
