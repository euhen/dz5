%% coding: utf-8
%1. Написать библиотеку для кэширования

-module(my_cache).
-export([create/1]).
-export([insert_i/4]).
-export([lookup_i/2]).
-export([delete_obsolete/1]).
-export([lookup_by_date_i/3]).
-include_lib("stdlib/include/ms_transform.hrl").

-define(SECOND, 1).

create(TableName) ->
    TableName = ets:new(TableName, [named_table]),
    ok.

insert_i(TableName, Key, Value, MaxTime) ->
    Now = erlang:system_time(?SECOND),
    ExpTime = MaxTime + erlang:system_time(?SECOND),
    true = ets:insert(TableName, {Key, Value, Now, ExpTime}),
    ok.

lookup_i(TableName, GivenKey) ->
    Now = erlang:system_time(?SECOND),
    MatchSpec = ets:fun2ms(
        fun({Key, Value, AddTime, ExpTime})
            when ExpTime > Now, Key =:= GivenKey ->
                Value
        end
    ),
    case ets:select(TableName, MatchSpec) of
        [Value] -> {ok, Value};
        [] -> {error, undefined}
    end.

delete_obsolete(TableName) ->
    Now = erlang:system_time(?SECOND),
    MatchSpec = ets:fun2ms(
        fun({_Key, _Value, _Now, ExpTime})
            when ExpTime =< Now ->
                ok
        end
    ),
    ets:select_delete(TableName, MatchSpec),
    ok.

lookup_by_date_i(TableName, DateFrom, DateTo) ->
    Now = erlang:system_time(?SECOND),
    MatchSpec = ets:fun2ms(
        fun({Key, Value, AddTime, ExpTime})
            when DateFrom =< AddTime, AddTime =< DateTo, ExpTime > Now ->
                {Key, Value}
        end
    ),
    case ets:select(TableName, MatchSpec) of
        [{_,Value}] -> {ok, Value};
        [_|_]=Value -> {ok, Value};
        [] -> {error, undefined}
    end.
    
