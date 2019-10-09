-module(storage).

-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
    [].

% add(Key, Value, []) ->
%     [{Key, Value}];
% add(Key, Value, [{K2, V2} | R]) when K2 < Key ->
%     [{K2, V2} | add(Key, Value, R)];
% add(Key, Value, Store) ->
%     [{Key, Value}|Store].
add(Key, Value, Store) ->
    [{Key, Value}|Store].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    split(From, To, Store, [], []).

split(_From, _To, [], Upd, Rest) ->
    {Upd, Rest};
split(From, To, [{K, V}|R], Upd, Rest) ->
    case key:between(K, From, To) of
        true ->
            split(From, To, R, [{K, V}|Upd], Rest);
        false ->
            split(From, To, R, Upd, [{K, V}|Rest])
    end.

merge(Entries, Store) ->
    lists:foldl(fun({K, V}, Acc) -> [{K, V}|Acc] end, Store, Entries).

