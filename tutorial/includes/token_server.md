```erlang linenums="1"
start(N) ->
  spawn(?MODULE, loop, [N]).

loop(OwnTok, NextTok) ->
  receive
    {Clt, 0} ->
      Clt ! NextTok,
      loop(OwnTok, NextTok + 1) % Increment token.
  end.
```