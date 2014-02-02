#! /usr/bin/env escript

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(1),
    etap:fun_is(
        fun({'EXIT', {badarg, _}}) -> true; (_) -> false end,
        (catch skeleton:skeleton(foo)),
        "Skeleton raised a badarg as expected."
    ),
    etap:end_tests().

