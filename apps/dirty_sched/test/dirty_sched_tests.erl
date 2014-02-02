-module(dirty_sched_tests).

-include_lib("eunit/include/eunit.hrl").


double_test() ->
    ?assertEqual(2, dirty_sched:double(1)).


triple_defer_test() ->
    ?assertEqual(3, dirty_sched:triple_defer(1)).


triple_direct_test() ->
    ?assertEqual(3, dirty_sched:triple_direct(1)).
