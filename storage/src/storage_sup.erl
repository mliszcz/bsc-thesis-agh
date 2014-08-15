%% @author Michal Liszcz
%% @doc Storage Server node root supervisor

-module(storage_sup).
-include("shared.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% this should be stopped by application master automatically

% stop() ->
% 	log:info("termintating itself!"),
% 	% log:info("children ~p", [supervisor:which_children(whereis(?MODULE))] ),
% 	log:info("terminated"),
% 	% exit(whereis(?MODULE), kill),
% 	% gen_server:cast(storage_http_srv, stop).
% 	% io:format("terminated~n", []),
% 	% supervisor:terminate_child(?MODULE, ?HTTP_SERVER),
% 	% io:format("terminated~n", []).
% 	ok.																%% @FIXME

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
	Children = [
		?CHILD(?UUID_SERVER, worker),
		?CHILD(?CORE_SERVER, worker),
		?CHILD(?AUTH_SERVER, worker),
		?CHILD(?DIST_SERVER, worker),
		?CHILD(?HTTP_SERVER, worker)
	],
	{ok, { {one_for_one, 5, 10}, Children} }.
