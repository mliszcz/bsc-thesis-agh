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
	log:info("remote scan start!"),
	RemoteNodes = remote_scan(sets:add_element(InitialNode, sets:new())),
	log:info("remote scan done!"),
	broadcast(RemoteNodes, ?DIST_SERVER, {hello, node()}),	% broadcast own name

	{ok, RemoteNodes}.

handle_call({request, #request{type=create, path=Path, data=Data}=Request},
	From, State) ->
	log:info("creating ~s", [Path]),
	spawn_link(
		fun() ->
			% TODO sort this list
			% RequiredCap = byte_size(Data),
			LeastFilled = node(),
			% [{LeastFilled, _Fill} | _Tail ] = broadcall(State, ?DIST_SERVER, {request_storage, RequiredCap}),
			gen_server:cast({?CORE_SERVER, LeastFilled}, {request, Request, From})
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


handle_call({state_info}, From, State) ->
	{reply, {ok, State}, State}.

% handle_call({request_storage, RequiredCap}, _From, State) ->
% 	case RequiredCap =< (globals:get(capacity)-globals:get(fill)) of
% 		true -> {reply, {ok, globals:get(fill)/globals:get(capacity)}, State};
% 		_ -> {reply, {error, storage_full}, State}
% 	end;

% handle_call({reserve_storage, RequiredCap}, _From, State) ->
% 	log:info("DIST: reserving...~n"),
% 	globals:set(fill, globals:get(fill)+RequiredCap),
% 	log:info("DIST: reserved!~n"),
% 	{reply, {ok, reserved}, State}.

% handle_cast(#request{} = _Request, State) ->
% 	% handle_request(Request, none),
% 	{noreply, State};

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
	Online = sets:from_list(lists:map(fun({Node, Res}) -> Node end, State)),
	Offline = sets:subtract(RemoteNodes, Online),
	AllNodes = sets:union(lists:map(fun({Node, Res}) -> Res end, State)),
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
				{ok,	Res}	-> Acc++{Node, Res};
				{error,	_}		-> Acc
			catch
				error:{timeout, _} -> Acc
			end
		end,
		[], RemoteNodes).

% least_filled_node(RemoteNodes, RequiredCap) ->
% 	Fills = broadcall(RemoteNodes, ?DIST_SERVER, {request_storage, RequiredCap}),
% 	[H | _] = Fills,
% 	return H.



% dispatch_put(RemoteNodes, Process, {request, Request, ReplyTo}=Message) ->
% 	{_, BestNode, _} = sets:fold(
% 		fun(Node, {Found, MinNode, MinFill}) ->
% 			case Found of
% 				true -> {Found, MinNode, MinFill};
% 				false ->
% 					case gen_server:call({Process, Node}, {request, Request#rreq{action=fnd}}) of
% 						{true, found} -> {true, Node, 0};
% 						{false, PercentFill} when PercentFill <  MinFill -> {false, Node, PercentFill};
% 						{false, PercentFill} when PercentFill >= MinFill -> {false, MinNode, MinFill}
% 					end
% 			end
% 		end,
% 		{false, none, 100}, RemoteNodes),
% 	log:info("DIST: dispatching PUT to ~p~n", [BestNode]),
% 	gen_server:cast({Process, BestNode}, Message).
