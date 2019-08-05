-module(cache_server_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(TableName, test_table).
-define(DropInterval, 3600).

%% Common Test callbacks
all() ->
	[
        insert,
        lookup,
        lookup_by_date
    ].

init_per_testcase(Case, Config) ->
    {ok, Pid} = cache_server:start_link(?TableName, [{drop_interval, ?DropInterval}]),
    ok = cache_server:insert(?TableName, "Key1", "Value1", 600),
    Config.

%% test cases
insert(Config) ->
    ok = cache_server:insert(?TableName, "Key2", "Value2", 600).

lookup(Config) ->
    {ok, Value} = cache_server:lookup(?TableName, "Key1").

lookup_by_date(Config) ->
    DateFrom1 = {{2015,1,1},{00,00,00}},
    DateTo1 = {{2015,1,10},{23,59,59}},
    error = cache_server:lookup_by_date(?TableName, DateFrom1, DateTo1),
    DateFrom2 = {{2019,1,1},{00,00,00}},
    DateTo2 = {{2020,1,10},{23,59,59}},
    {ok, Value} = cache_server:lookup_by_date(?TableName, DateFrom2, DateTo2).
