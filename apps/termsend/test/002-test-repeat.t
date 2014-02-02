#! /usr/bin/env escript

wait_for(Mesg) ->
    receive
        foo ->
            etap:ok(true, "foo message was received."),
            ok;
        Other ->
            etap:diag("Ignoring message: ~p", [Other]),
            wait_for(Mesg)
        after 1000 ->
            ok
    end.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(2),
    etap:is(termsend:repeat(self(), foo), ok, "repeat returned ok"),
    wait_for(foo),
    etap:end_tests().

