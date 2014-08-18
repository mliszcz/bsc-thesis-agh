%% @author Michal Liszcz
%% @doc Filesystem access

-module(files).

-export([ resolve_name/1, read/1, write/2, delete/1 ]).


resolve_name(RelativeFileName) ->
	filename:join([util:get_env(core_node_dir), RelativeFileName]).

-ifdef(nofileio).
read(_RelativeFileName) -> {ok, << >>}.
-else.
read(RelativeFileName) -> file:read_file(resolve_name(RelativeFileName)).
-endif.


-ifdef(nofileio).
write(_RelativeFileName, _BinaryData) -> pass.
-else.
write(RelativeFileName, BinaryData) -> file:write_file(resolve_name(RelativeFileName), BinaryData).
-endif.


-ifdef(nofileio).
delete(_RelativeFileName) -> pass.
-else.
delete(RelativeFileName) -> file:delete(resolve_name(RelativeFileName)).
-endif.
