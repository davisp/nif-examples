
all:
	./rebar compile

clean:
	./rebar clean

check:
	./rebar compile eunit
