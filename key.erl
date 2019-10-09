-module(key).
-export([generate/0, between/3]).

%-define(Bigval, 1000000000).
-define(Bigval, 100).

generate() ->
    rand:uniform(?Bigval).

between(Key, From, To) when From > To ->
    (Key =< To) or (Key > From);
between(_, From, From) ->
    true;
between(Key, From, To) ->
    (Key > From) and (Key =< To).

