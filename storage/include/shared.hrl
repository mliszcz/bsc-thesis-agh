%% @author Michal
%% @doc Shared data structures

-define(TIMEOUT, 30000).

% node server modules
-define(CORE_SERVER, storage_core_srv).
-define(DIST_SERVER, storage_dist_srv).
-define(HTTP_SERVER, storage_http_srv).

-record(filedesc, {
	user,
	path,
	size,
	mode = private, % mode = private | public
	last_access,
	internal_id
	}).

-record(file, {
	id, 			% integer()
	vpath, 			% list()
	owner, 			% integer()
	bytes, 			% integer()
	location, 		% list()
	access_mode, 	% integer()
	create_time 	% integer()
	}).

%% improved request

-record(request, {
	type,	% action = create | read | update | delete | list | find
	user,	% user = string()
	path = none,
	data = none,
	opts = none
	}).
