
-module(utils).

%% API
-export([datetime_to_timestamp/1]).

datetime_to_timestamp(
    {{_Year, _Month, _Day},{_Hours, _Minutes, _Seconds}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
