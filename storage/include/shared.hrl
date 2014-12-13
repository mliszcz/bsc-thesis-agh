%% @author Michal
%% @doc Shared data structures

-define(TIMEOUT, 5000).


% node server modules
-define(UUID_SERVER, storage_uuid_srv).
-define(CORE_SERVER, storage_core_srv).
-define(AUTH_SERVER, storage_auth_srv).
-define(DIST_SERVER, storage_dist_srv).
-define(HTTP_SERVER, storage_http_srv).


-ifdef(nolog).
-define(LOG_INFO(M), 		true).
-define(LOG_INFO(M, D), 	true).
-define(LOG_WARN(M), 		true).
-define(LOG_WARN(M, D), 	true).
-define(LOG_ERROR(M), 		true).
-define(LOG_ERROR(M, D), 	true).
-else.
-define(LOG_INFO(M), 		log:info(M)).
-define(LOG_INFO(M, D), 	log:info(M, D)).
-define(LOG_WARN(M), 		log:warn(M)).
-define(LOG_WARN(M, D), 	log:warn(M, D)).
-define(LOG_ERROR(M), 		log:error(M)).
-define(LOG_ERROR(M, D), 	log:error(M, D)).
-endif.


-ifdef(noauth).
-define(AUTH_CALL(N, R), {ok, authenticated}).
-else.
-define(AUTH_CALL(N, R), gen_server:call({?AUTH_SERVER, N}, {authenticate, R})).
-endif.


-ifdef(nopersistentdb).
-define(SQLITE3_CONNECT(DB, F), sqlite3:open(DB, [in_memory])).
-else.
-define(SQLITE3_CONNECT(DB, F), sqlite3:open(DB, [{file, F}])).
-endif.


-record(file, {
	id, 			% list()
	owner 			:: nonempty_string(), 			% list()
	vpath 			:: nonempty_string(), 			% list()
	bytes 			:: integer(), 					% integer()
	access_mode 	:: integer(), 					% integer()
	create_time 	:: integer() 					% integer()
	}).


-record(user, {
	id, 			% list()
	name			:: nonempty_string(), 			% list()
	secret 			:: nonempty_string(), 			% list()
	create_time 	:: integer(), 					% integer()
	storage_grant 	% integer()
	}).


-record(action, {
	id, 			% list()
	user_id 		:: nonempty_string(), 	% list()
	file_id 		:: nonempty_string(), 	% list()
	weight 			:: integer(), 			% integer()
	action_time 	:: integer(), 			% integer()
	action_type 	:: nonempty_string() 	% "create" | "read" | "update" | "delete" | "list" | "find" % integer() = C|R|U|D|L|F
	}).


-record(request, {
	type 					:: 'create' | 'read' | 'update' | 'delete' | 'list' | 'find', 		% create | read | update | delete | list | find
	user 					:: nonempty_string(), 									% list()
	addr = {none, none} 	:: { nonempty_string(), nonempty_string() }, 			% {owner, vpath}
	hmac = none 			:: string(), 											% list()
	data = none 			:: binary(), 											% binary()
	opts = none 			:: term() 												% -unused-
	}).
