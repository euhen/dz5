-module(my_cache_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(my_cache, [create/1, insert_i/4, lookup_i/2, delete_obsolete/1, lookup_by_date_i/3]).

create_test_() ->
    N = tt1,
    ?_assert(create(N) =:= ok).

insert_i_test_() ->
    N = tt1,
    ?_assert(insert_i(N, "Key1", "Value1", 0) =:= ok),
    ?_assert(insert_i(N, "Key2", "Value2", 2) =:= ok),
    ?_assert(insert_i(N, "Key3", "Value3", 60) =:= ok).

lookup_i_test_() ->
    N = tt1,
    ?_assert(lookup_i(N, "Key1") =:= {error, undefined}),
    ?_assert(lookup_i(N, "Key2") =:= {ok, "Value2"}),
    timer:sleep(4 * 1000),
    ?_assert(lookup_i(N, "Key2") =:= {error, undefined}),
    ?_assert(lookup_i(N, "Key3") =:= {ok, "Value3"}).

delete_obsolete_test_() ->
    N = tt1,
    ?_assert(delete_obsolete(N) =:= ok).
