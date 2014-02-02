-module(resource_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertEqual(0, resources:count()),
    {ok, Res} = resources:create(90),
    ?assertEqual(1, resources:count()),
    ?assertEqual(90, resources:read(Res)),
    Fun = fun() ->
        {ok, Res2} = resources:create(18),
        ?assertEqual(2, resources:count()),
        ?assertEqual(18, resources:read(Res2))
    end,
    Fun(),
    erlang:garbage_collect(),
    ?assertEqual(90, resources:read(Res)),
    ?assertEqual(1, resources:count()).
