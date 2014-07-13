%% @author Michal Liszcz
%% @doc Filesystem access

-module(files).

-export([ resolve_name/1, read/1, write/2, delete/1 ]).

resolve_name(RelativeFileName) ->
	filename:join([util:get_env(core_node_dir), RelativeFileName]).

read(RelativeFileName) ->
	file:read_file(resolve_name(RelativeFileName)).

write(RelativeFileName, BinaryData) ->
	file:write_file(resolve_name(RelativeFileName), BinaryData).

delete(RelativeFileName) ->
	file:delete(resolve_name(RelativeFileName)).
