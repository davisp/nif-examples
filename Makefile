
all:
	./rebar compile

clean:
	./rebar clean
	rm -rf apps/*/.eunit/

check:
	./rebar compile eunit
