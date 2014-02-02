-module(termsend_tests).

-include_lib("eunit/include/eunit.hrl").

next_message() ->
    receive
        Msg -> Msg
    after 1000 ->
        erlang:errot(timeout)
    end.

basic_test() ->
    ?assertEqual(ok, termsend:repeat(self(), foo)),
    ?assertEqual(foo, next_message()).
