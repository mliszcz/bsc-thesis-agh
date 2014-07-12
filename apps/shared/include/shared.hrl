%% @author Michal
%% @doc Shared data structures


% -define(STORAGE_PROC, storage).
% -define(SYSTEM_PROC, system).
-define(TIMEOUT, 1000).

% node server modules
-define(CORE_SERVER, storage_core_srv).
-define(DIST_SERVER, storage_dist_srv).
-define(HTTP_SERVER, storage_http_srv).


% -record(file, {
% 			   		v_path	= "/empty",	% string			// THIS AND THIS
% 			   		owner_id,			% string (uuid)		// FORMS PRIMARY KEY

% 					local_id,			% string (uuid) 	// **ONLY INTERNAL USE !!!!**

% 					size,				% in bytes
% 					last_access			% util:timestamp()
					
% 					}).

-record(filedesc, {
	user,
	path,
	size,
	mode = private, % mode = private | public
	last_access,
	internal_id
	}).

%% improved request

-record(request, {
	type,	% action = create | read | update | delete | list | find
	user,	% user = string()
	path = none,
	data = none,
	opts = none
	}).

% -record(create_opts, {
% 					  	force_loc = false,	% atom true | false
% 						data				% raw data
% 						}).

% -record(write_opts, {
% 						v_path = false,	% false = no update
% 						data = false	%
% 						}).

% -record(move_opts, {
% 						new_loc		% atom (node)
% 						}).