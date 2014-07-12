%% @author Michal Liszcz
%% @doc Storage Server communication module

-module(storage_dist_srv).
-include("shared.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
	log:info("starting"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->

	% globals:set(capacity, util:get_env(dist_storage_cap)),

	InitialNode = list_to_atom(util:get_env(dist_initial_node)),
	log:info("performing remote scan"),
	RemoteNodes = remote_scan(sets:add_element(InitialNode, sets:new())),
	log:info("remote scan done!"),
	broadcast(RemoteNodes, ?DIST_SERVER, {hello, node()}),	% broadcast own name

	{ok, RemoteNodes}.

handle_call({request, #request{type=create, path=Path, data=Data}=Request},
	From, State) ->
	log:info("creating ~s", [Path]),
	spawn_link(
		fun() ->
			Size = byte_size(Data),
			Fills = broadcall(State, ?CORE_SERVER, {reserve, Size}),
			{_, Best} = lists:min(lists:map(fun({Node, Fill}) -> {Fill,Node} end, Fills)),
			gen_server:cast({?CORE_SERVER, Best}, {request, Request, From}),
			broadcast(sets:del_element(Best, State), ?CORE_SERVER, {release, Size})
		end),
	{noreply, State};

handle_call({request, #request{type=read, path=Path}=Request},
	From, State) ->
	log:info("reading ~s", [Path]),
	spawn_link(
		fun() ->
			broadcast(State, ?CORE_SERVER, {request, Request, From})
		end),
	{noreply, State};

handle_call({request, #request{type=update, path=Path}=Request},
	From, State) ->
	log:info("updating ~s", [Path]),
	spawn_link(
		fun() ->
			broadcast(State, ?CORE_SERVER, {request, Request, From})
		end),
	{noreply, State};

handle_call({request, #request{type=delete, path=Path}=Request},
	From, State) ->
	log:info("deleting ~s", [Path]),
	spawn_link(
		fun() ->
			broadcast(State, ?CORE_SERVER, {request, Request, From})
		end),
	{noreply, State};


handle_call({state_info}, _From, State) ->
	{reply, {ok, State}, State}.

handle_cast({hello, Node}, State) ->
	log:info("~p has joined the cluster!", [Node]),
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

%% @doc Scans all known remote nodes to find new ones
remote_scan(RemoteNodes) ->
	State = broadcall(sets:del_element(node(), RemoteNodes), ?DIST_SERVER, {state_info}),
	Online = sets:from_list(lists:map(fun({Node, _Res}) -> Node end, State)),
	Offline = sets:subtract(RemoteNodes, Online),
	AllNodes = sets:union(lists:map(fun({_Node, Res}) -> Res end, State)),
	sets:add_element(node(), sets:subtract(AllNodes, Offline)).


broadcast(RemoteNodes, Process, Message) ->
	sets:fold(
		fun(Node, _Acc) ->
			gen_server:cast({Process, Node}, Message)
		end,
		[], RemoteNodes).

broadcall(RemoteNodes, Process, Message) ->
	sets:fold(
		fun(Node, Acc) ->
			try gen_server:call({Process, Node}, Message) of
				{ok,	Res}	-> Acc++[{Node, Res}];
				{error,	_}		-> Acc
			catch
				error:{timeout, _} -> Acc
			end
		end,
		[], RemoteNodes).
