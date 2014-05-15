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
	io:format("dist gen server ONLINE~n", []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->

	globals:set(capacity, util:get_env(dist_storage_cap)),

	InitialNode = list_to_atom(util:get_env(dist_initial_node)),

	RemoteNodes = sets:new(),
	RemoteNodes = sets:add_element(InitialNode, RemoteNodes),
	RemoteNodes = remote_scan(RemoteNodes),
	say_hello(RemoteNodes),

	{ok, RemoteNodes}.

handle_call(#rreq{} = Request, From, State) ->
	% handle_request(Request, From),
	% {noreply, State};
	{reply, ok, State};

handle_call({get_remote_nodes}, _From, State) ->
	{reply, {ok, State}, State};

handle_call({request_storage, RequiredCap}, _From, State) ->
	case RequiredCap =< (globals:get(capacity)-globals:get(fill)) of
		true -> {reply, {ok, globals:get(fill)/globals:get(capacity)}, State};
		_ -> {reply, {error, storage_full}, State}
	end;

handle_call({reserve_storage, RequiredCap}, _From, State) ->
	io:format("~w: reserving on system!~n", [erlang:localtime()]),
	globals:set(fill, globals:get(fill)+RequiredCap),
	io:format("~w: reserved, responding...~n", [erlang:localtime()]),
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
			try gen_server:call({?DIST_SERVER, Node}, {get_remote_nodes}) of
				{ok, NodeSet} -> sets:union(Acc, NodeSet)
			catch
				_:_ -> Acc
			end
		end,
		RemoteNodes, RemoteNodes).

%% @doc Broadcasts own name to all nodes in the system
say_hello(RemoteNodes) ->
	sets:fold(
		fun(Node, _Acc) ->
			gen_server:cast({?DIST_SERVER, Node}, {hello, node()})
		end,
		[], RemoteNodes).

