-module(termsend_threaded_tests).

-include_lib("eunit/include/eunit.hrl").


repeat_messages(Dst) ->
    receive
        Msg ->
            Dst ! {repeated, Msg}
    end,
    repeat_messages(Dst).


next_message() ->
    receive
        Msg -> Msg
    after 1000 ->
        erlang:errot(timeout)
    end.


send_to_self_test() ->
    ?assertEqual(ok, termsend_threaded:send_to_self()),
    ?assertEqual(foo, next_message()).


send_to_pid_test() ->
    Self = self(),
    Pid = spawn_link(fun() -> repeat_messages(Self) end),
    ?assertEqual(ok, termsend_threaded:send_to_pid(Pid)),
    ?assertEqual({repeated, foo}, next_message()).

