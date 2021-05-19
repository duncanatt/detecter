# detectEr Tutorial

## sHML Lexer

A Pygments lexer that can be integrated in  MkDocs to highlight sHML code. The
code was inspired from [here](https://github.com/tremor-rs/tremor-mkdocs-lexer)
and [here](https://github.com/facelessuser/pymdown-extensions).

The lexer caters for sHML script version 0.9.0.

## Set up on macOS or Linux

    $ python ./setup.py install  # To remove pip uninstall shml_lexer

## Testing Locally via Command Line

    $ pygmentize -l shml /path/to/file.shml

## Set up on MkDocs

No set up is required for local testing.
The lexer module will be automatically registered with MkDocs and made available.
sHML scripts are highlighted in Markdown as follows:

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