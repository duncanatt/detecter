Adapted from: https://github.com/tremor-rs/tremor-mkdocs-lexer

and https://github.com/facelessuser/pymdown-extensions


    python ./setup.py install

    pygmentize -l shml /path/to/file.shml


    ```shml
    with
    calc_server:loop(_)
    monitor
    and([_Launcher <- _Server, calc_server:loop(_)]
    max(X.
    and(
    [_Server ? {_, {add, A, B}}] and(
    [_Server ! {add, Res} when Res =/= A + B]ff,
    [_Server ! {add, Res} when Res =:= A + B]X
    ),
    [_Server ? {_, {_, _, _}}] and(
    [_Server ! {_, _}]X
    ),
    [_Server ? {_, stop}] and(
    [_Server ! {ok, stopped}]X
    )
    )
    )
    ).
    ```