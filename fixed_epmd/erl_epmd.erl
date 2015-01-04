%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(erl_epmd).

-behaviour(gen_server).

-ifdef(DEBUG).
-define(port_please_failure(), io:format("Net Kernel 2: EPMD port please failed at ~p:~p~n", [?MODULE,?LINE])).
-define(port_please_failure2(Term), io:format("Net Kernel 2: EPMD port please failed at ~p:~p [~p]~n", [?MODULE,?LINE,Term])).
-else.
-define(port_please_failure(), noop).
-define(port_please_failure2(Term), noop).
-endif.

%% External exports
-export([start/0, start_link/0, stop/0, port_please/2, 
	 port_please/3, names/0, names/1,
	 register_node/2, open/0, open/1, open/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-import(lists, [reverse/1]).

-record(state, {socket, port_no = -1, name = ""}).
-type state() :: #state{}.

-include("inet_int.hrl").
-include("erl_epmd.hrl").


-define(FixedPortList, [
	{"ds1",9101},
	{"ds2",9201},
	{"ds3",9301},
	{"ds4",9401}
]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, erl_epmd}, ?MODULE, [], []).


start_link() ->
    gen_server:start_link({local, erl_epmd}, ?MODULE, [], []).


stop() ->
    gen_server:call(?MODULE, stop, infinity).


%% Lookup a node "Name" at Host
%% return {port, P, Version} | noport
%%

port_please(Node, Host) ->
  port_please(Node, Host, infinity).

port_please(Node,HostName, Timeout) when is_atom(HostName) ->
  port_please1(Node,atom_to_list(HostName), Timeout);
port_please(Node,HostName, Timeout) when is_list(HostName) ->
  port_please1(Node,HostName, Timeout);
port_please(Node, EpmdAddr, Timeout) ->
  get_port(Node, EpmdAddr, Timeout).



port_please1(Node,HostName, Timeout) ->
      get_port(Node, HostName, Timeout).

names() ->
    {ok, H} = {ok, "127.0.0.1"},
    names(H).

names(HostName) when is_atom(HostName); is_list(HostName) ->
      get_names(HostName);

names(EpmdAddr) ->
  get_names(EpmdAddr).


register_node(Name, PortNo) ->
    gen_server:call(erl_epmd, {register, Name, PortNo}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

-spec init(_) -> {'ok', state()}.

init(_) ->
    {ok, #state{socket = -1}}.
	    
%%----------------------------------------------------------------------

-type calls() :: 'client_info_req' | 'stop' | {'register', term(), term()}.

-spec handle_call(calls(), term(), state()) ->
        {'reply', term(), state()} | {'stop', 'shutdown', 'ok', state()}.

handle_call({register, Name, PortNo}, _From, State) ->
    case State#state.socket of
	P when P < 0 ->
	    case do_register_node(Name, PortNo) of
		{alive, Socket, Creation} ->
		    S = State#state{socket = Socket,
				    port_no = PortNo,
				    name = Name},
		    {reply, {ok, Creation}, S};
		Error ->
		    {reply, Error, State}
	    end;
	_ ->
	    {reply, {error, already_registered}, State}
    end;

handle_call(client_info_req, _From, State) ->
    Reply = {ok,{r4,State#state.name,State#state.port_no}},
    {reply, Reply, State};
  
handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

%%----------------------------------------------------------------------

-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------

-spec handle_info(term(), state()) -> {'noreply', state()}.

handle_info({tcp_closed, Socket}, State) when State#state.socket =:= Socket ->
    {noreply, State#state{socket = -1}};
handle_info(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------

-spec terminate(term(), state()) -> 'ok'.

terminate(_, #state{socket = Socket}) when Socket > 0 ->
    ok;
terminate(_, _) ->
    ok.

%%----------------------------------------------------------------------

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

get_epmd_port() ->
    case init:get_argument(epmd_port) of
	{ok, [[PortStr|_]|_]} when is_list(PortStr) ->
	    list_to_integer(PortStr);
	error ->
	    ?erlang_daemon_port
    end.
	    
%%
%% Epmd socket
%%
% open() -> open({127,0,0,1}). % The localhost IP address.
% open({A,B,C,D}=EpmdAddr) when ?ip(A,B,C,D) -> {ok, 0};
% open({A,B,C,D,E,F,G,H}=EpmdAddr) when ?ip6(A,B,C,D,E,F,G,H) -> {ok, 0}.
% open({A,B,C,D}=EpmdAddr, Timeout) when ?ip(A,B,C,D) -> {ok, 0};
% open({A,B,C,D,E,F,G,H}=EpmdAddr, Timeout) when ?ip6(A,B,C,D,E,F,G,H) -> {ok, 0}.
% close(Socket) -> ok.

open() -> open({127,0,0,1}). % The localhost IP address.
open({A,B,C,D}=EpmdAddr) when ?ip(A,B,C,D) ->
gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet]);
open({A,B,C,D,E,F,G,H}=EpmdAddr) when ?ip6(A,B,C,D,E,F,G,H) ->
gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet6]).
open({A,B,C,D}=EpmdAddr, Timeout) when ?ip(A,B,C,D) ->
gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet], Timeout);
open({A,B,C,D,E,F,G,H}=EpmdAddr, Timeout) when ?ip6(A,B,C,D,E,F,G,H) ->
gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet6], Timeout).
close(Socket) ->
gen_tcp:close(Socket).


do_register_node(NodeName, TcpPort) ->
case open() of
	{ok, Socket} ->
	Name = to_string(NodeName),
	Extra = "",
	Elen = length(Extra),
	Len = 1+2+1+1+2+2+2+length(Name)+2+Elen,
	gen_tcp:send(Socket, [?int16(Len), ?EPMD_ALIVE2_REQ,
		?int16(TcpPort),
		$M,
		0,
		?int16(epmd_dist_high()),
		?int16(epmd_dist_low()),
		?int16(length(Name)),
		Name,
		?int16(Elen),
		Extra]),
	wait_for_reg_reply(Socket, []);
	Error ->
	Error
end.

wait_for_reg_reply(Socket, SoFar) ->
receive
	{tcp, Socket, Data0} ->
	case SoFar ++ Data0 of
		[$y, Result, A, B] ->
		case Result of
			0 ->
			{alive, Socket, ?u16(A, B)};
			_ ->
			{error, duplicate_name}
		end;
		Data when length(Data) < 4 ->
		wait_for_reg_reply(Socket, Data);
		Garbage ->
		{error, {garbage_from_epmd, Garbage}}
	end;
	{tcp_closed, Socket} ->
	{error, epmd_close}
	after 10000 ->
		gen_tcp:close(Socket),
		{error, no_reg_reply_from_epmd}
	end.


epmd_dist_high() ->
    case os:getenv("ERL_EPMD_DIST_HIGH") of
	false ->
	   ?epmd_dist_high; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when is_integer(N), N < ?epmd_dist_high ->
		    N;
		_ ->
		   ?epmd_dist_high
	    end
    end.

epmd_dist_low() ->
    case os:getenv("ERL_EPMD_DIST_LOW") of
	false ->
	   ?epmd_dist_low; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when is_integer(N), N > ?epmd_dist_low ->
		    N;
		_ ->
		   ?epmd_dist_low
	    end
    end.
		    



%%
%% Lookup a node "Name" at Host
%%

get_port(Node, EpmdAddress, Timeout) when is_list(Node) ->
	case proplists:get_value(Node, ?FixedPortList) of
		undefined ->  noport;
		Port ->
			Version = 5, %best_version(0, 5),
			{port, Port, Version}
	end;
get_port(Node, EpmdAddress, Timeout) when is_atom(Node) ->
	get_port(atom_to_list(Node), EpmdAddress, Timeout).



best_version(Low, High) ->
    OurLow =  epmd_dist_low(),
    OurHigh =  epmd_dist_high(),
    select_best_version(OurLow, OurHigh, Low, High).

%%% We silently assume that the low's are not greater than the high's.
%%% We should report if the intervals don't overlap.
select_best_version(L1, _H1, _L2, H2) when L1 > H2 ->
    0;
select_best_version(_L1, H1, L2, _H2) when L2 > H1 ->
    0;
select_best_version(_L1, H1, L2, _H2) when L2 > H1 ->
    0;
select_best_version(_L1, H1, _L2, H2) ->
    erlang:min(H1, H2).



%%
%% Creates a (flat) null terminated string from atom or list.
%%

to_string(S) when is_atom(S) -> atom_to_list(S);
to_string(S) when is_list(S) -> S.

%%
%% Find names on epmd
%%
%%
get_names(EpmdAddress) ->
	{ok, ?FixedPortList}.
