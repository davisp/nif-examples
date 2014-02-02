
all:
	./rebar compile

clean:
	./rebar clean
	rm -rf appps/*/priv/*.so
	rm -rf apps/*/.eunit/

check:
	./rebar compile eunit
