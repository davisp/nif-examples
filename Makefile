
all:
	./rebar compile

clean:
	./rebar clean

check:
	ERL_LIBS=./apps prove -v apps/*/test/*.t
