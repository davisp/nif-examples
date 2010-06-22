#! /usr/bin/env escript

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(1),
    etap:can_load(skeleton),
    etap:end_tests().

