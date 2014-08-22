%% @author Michal Liszcz
%% @doc 128bit unique identifier generator
%% (48bit milisecond timestamp + 48bit mac + 32bit random)

-module(storage_uuid_srv).
-include("shared.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, generate/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).

generate() ->
	gen_server:call(?SERVER, generate).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	?LOG_INFO("starting uuid generator"),
	{ok, get_mac_address(util:get_env(uuid_interface_name))}.

handle_call(generate, From, State) ->
	spawn(fun() ->
			Now = util:timestamp(),
			Rand = crypto:rand_bytes(4),
			BinId = << Now:48/integer, State/binary, Rand/binary >>,
			gen_server:reply(From, util:binary_to_hex_string(BinId))
		end),
	{noreply, State}.

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
 
get_mac_address(InterfaceName)
	when is_list(InterfaceName) ->
	try
		{ok, Interfaces} = inet:getifaddrs(),
		ListMac = proplists:get_value(hwaddr, proplists:get_value(InterfaceName, Interfaces)),
		<< IntMac:48/integer >> = list_to_binary(ListMac),	% sanity check
		list_to_binary(ListMac)
	catch
		_:_ ->
			?LOG_ERROR("unable to get MAC address for interface ~s", [InterfaceName]),
			<< 0:48/integer >>
	end.
