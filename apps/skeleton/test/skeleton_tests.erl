-module(skeleton_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertException(error, badarg, skeleton:skeleton(foo)).
