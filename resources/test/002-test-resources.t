#! /usr/bin/env escript

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(6),
    etap:is(resources:count(), 0, "No resources created yet."),
    {ok, Res} = resources:create(90),
    etap:is(resources:count(), 1, "One resource created."),
    etap:is(resources:read(Res), 90, "Correct resource id."),
    Func = fun() ->
        {ok, Res2} = resources:create(18),
        etap:is(resources:count(), 2, "Two resources created."),
        etap:is(resources:read(Res2), 18, "Correct resource id.")
    end,
    Func(),
    erlang:garbage_collect(),
    etap:is(resources:count(), 1, "Destroyed a resource."),
    etap:end_tests().


