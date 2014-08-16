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
	log:info("dist starting"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	log:info("shutdown"),
	gen_server:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->

	% process_flag(trap_exit, true),

	% globals:set(capacity, util:get_env(dist_storage_cap)),

	InitialNode = list_to_atom(util:get_env(dist_initial_node)),
	log:info("performing remote scan"),
	RemoteNodes = remote_scan(sets:add_element(InitialNode, sets:new())),
	log:info("remote scan done!"),
	broadcast(RemoteNodes, ?DIST_SERVER, {hello, node()}),	% broadcast own name

	{ok, RemoteNodes}.


handle_call({request, #request{type=create, path=Path}=Request},
	From, State) ->
	log:info("creating ~s", [Path]),
	spawn_link(
		fun() ->
			case gen_server:call({?AUTH_SERVER, node()}, {authenticate, Request}) of
				{error, _} -> gen_server:reply(From, {error, authenticaiton_failed});
				{ok, 	_} ->
					Size = byte_size(Request#request.data),
					Fills = broadcall(State, ?CORE_SERVER, {reserve, Size}),
					{_, Best} = lists:min(lists:map(fun({Node, Fill}) -> {Fill,Node} end, Fills)),
					% TODO - handle case when system is full (Fills may be empty)!
					gen_server:cast({?CORE_SERVER, Best}, {{request, Request}, From}),
					broadcast(sets:del_element(Best, State), ?CORE_SERVER, {release, Size})
				end
		end),
	{noreply, State};


handle_call({request, #request{type=update, path=Path}=Request},
	From, State) ->
	log:info("updating ~s", [Path]),
	spawn_link(
		fun() ->
			case gen_server:call({?AUTH_SERVER, node()}, {authenticate, Request}) of
				{error, _} -> gen_server:reply(From, {error, authenticaiton_failed});
				{ok, 	_} ->
					% prepare find request to locate node where file is kept
					% tampering with request and exposing dist call {broadcast, ...}
					% is huge security threat, but there is no other, simple way

					FindRequest = Request#request{type=find, data=none},

					try gen_server:call({?DIST_SERVER, node()},
						{broadcast, ?CORE_SERVER,
						{request, FindRequest}}, 4000) of 	% same timeout issue as in auth_serv:fetch_user
						
						{ok, Node} -> gen_server:cast({?CORE_SERVER, Node}, {{request, Request}, From})
					catch
						_:{timeout, _} -> pass
					end
			end
		end),
	{noreply, State};


handle_call({request, #request{type=Type}=Request}, From, State)
	when Type == read ; Type == delete ; Type == find ->

	spawn_link(
		fun() ->
			case gen_server:call({?AUTH_SERVER, node()}, {authenticate, Request}) of
				{error, _} -> gen_server:reply(From, {error, authenticaiton_failed});
				{ok, 	_} -> broadcast(State, ?CORE_SERVER, {{request, Request}, From})
			end
		end),
	{noreply, State};


handle_call({request, #request{type=list}=Request}, From, State) ->
	log:info("listing"),
	spawn_link(
		fun() ->
			case gen_server:call({?AUTH_SERVER, node()}, {authenticate, Request}) of
				{error, _} -> gen_server:reply(From, {error, authenticaiton_failed});
				{ok,	_} ->
					List = broadcall(State, ?CORE_SERVER, {{request, Request}, From}),
					gen_server:reply(From, {ok, List})
			end
		end),
	{noreply, State};


handle_call({broadcast, Process, Message}, From, State) ->
	spawn_link(
		fun() ->
			broadcast(State, Process, {Message, From})
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


terminate(Reason, _State) ->
	log:info("terminating dist due to ~p", [Reason]),
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
				_:{timeout, _} -> Acc;
				_:{{nodedown, _}, _} -> Acc;
				Type:Error ->
					log:error("broadcall failed on node ~p, reason: ~w:~w", [Node, Type, Error]),
					Acc
			end
		end,
		[], RemoteNodes).
