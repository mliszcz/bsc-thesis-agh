
all:
	./rebar compile

clean:
	./rebar clean

release:
	./rebar compile generate
	cp node.config storage/rel/storage/node.config
	echo "-sname ds" >> storage/rel/storage/releases/0.1.0/vm.args
	echo "-setcookie ciacho" >> storage/rel/storage/releases/0.1.0/vm.args
