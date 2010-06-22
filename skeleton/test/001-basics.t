#! /usr/bin/env escript

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(1),
    etap:loaded_ok(skeleton, "skeleton module."),
    etap:end_tests().

