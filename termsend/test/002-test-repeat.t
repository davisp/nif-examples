#! /usr/bin/env escript

wait_for(Parent, Mesg) ->
    receive
        foo ->
            etap:ok(true, "foo message was received.");
        Other ->
            etap:diag("Ignoring message: ~p", [Other]),
            wait_for(Parent, Mesg)
        after 1000 ->
            ok
    end,
    Parent ! finished.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(2),
    Parent = self(),
    Pid = spawn(fun() -> wait_for(Parent, foo) end),
    etap:diag("Self: ~p Other: ~p", [Parent, Pid]),
    etap:is(termsend:repeat(Pid, foo), ok, "repeat returned ok"),
    receive
        finished -> 
            ok
        after 1000 ->
            throw({error, timeout})
    end,
    etap:end_tests().

