%% coding: utf-8
%1. Написать кеширующий сервер.

-module(cache_server).
-behaviour(gen_server).

-import(my_cache, [create/1, insert_i/4, lookup_i/2,
    lookup_by_date_i/3, delete_obsolete/1]).
-import(utils, [datetime_to_timestamp/1]).

%% API.
-export([start_link/2]).

-export([insert/4]).
-export([lookup/2]).
-export([lookup_by_date/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API.

start_link(TableName, [{drop_interval, DropInterval}]) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, {TableName, DropInterval}, []).

insert(TableName, Key, Value, MaxTime)->
    gen_server:call({global, ?MODULE}, {insert, TableName, Key, Value, MaxTime}).

lookup(TableName, Key)->
    gen_server:call({global, ?MODULE}, {lookup, TableName, Key}).

lookup_by_date(TableName, DateFrom, DateTo)->
    gen_server:call({global, ?MODULE}, {lookup_by_date, TableName, DateFrom, DateTo}).

%% gen_server.

init({TableName, DropInterval}) ->
    ok = my_cache:create(TableName),
    %{ok, TRef} = timer:apply_interval(DropInterval, my_cache, delete_obsolete, [TableName]),
    {ok, []}.

handle_call({insert, TableName, Key, Value, MaxTime}, _From, State) ->
    ok = my_cache:insert_i(TableName, Key, Value, MaxTime),
    {reply, ok, State};

handle_call({lookup, TableName, Key}, _From, State) ->
    Response = my_cache:lookup_i(TableName, Key),
    case Response of
        {ok, Value} -> {reply, Response, State};
        {error, _} -> {reply, error, State}
    end;

handle_call({lookup_by_date, TableName, TupleDateFrom, TupleDateTo}, _From, State) ->
    DateFrom = utils:datetime_to_timestamp(TupleDateFrom),
    DateTo = utils:datetime_to_timestamp(TupleDateTo),
    Response = my_cache:lookup_by_date_i(TableName, DateFrom, DateTo),
    case Response of
        {ok, Value} -> {reply, Response, State};
        {error, _} -> {reply, error, State}
    end;

handle_call(_Message, _From, State) ->
    {reply, invalid_command, State}.    

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Message, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
