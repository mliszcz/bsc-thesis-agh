
all: clean compile

rel: release

compile:
	./rebar compile

clean:
	./rebar clean

release: clean
	./rebar compile generate
	cp node.config storage/rel/storage/node.config
	echo "-sname ds" >> storage/rel/storage/releases/0.1.0/vm.args
	echo "-setcookie ciacho" >> storage/rel/storage/releases/0.1.0/vm.args
