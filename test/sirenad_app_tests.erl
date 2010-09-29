-module(sirenad_app_tests).


-include_lib("eunit/include/eunit.hrl").


get_env1_test() ->
    ?assertError(_, sirenad_app:get_env(key1)),
    application:set_env(sirenad, key1, "exists"),
    ?assertEqual("exists", sirenad_app:get_env(key1)).


get_env2_test() ->
    ?assertEqual("default", sirenad_app:get_env(key2, "default")),
    application:set_env(sirenad, key2, "exists"),
    ?assertEqual("exists", sirenad_app:get_env(key2, "default")).
