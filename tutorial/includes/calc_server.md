```erlang linenums="1"
start(N) ->
  spawn(?MODULE, loop, [N]).

loop(Tot) ->
  receive
    {Clt, {add, A, B}} ->
      Clt ! {ok, A + B},
      loop(Tot + 1);

    {Clt, {mul, A, B}} ->
      Clt ! {ok, A * B},
      loop(Tot + 1);

    {Clt, stp} -> % Stop service.
      Clt ! {bye, Tot}
  end.
```