# This is a test

[comment]: <> (https://github.com/squidfunk/mkdocs-material/issues/873)



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

```shml
with
  'Elixir.Demo.CalcServer':loop(_)
monitor
  and([_Launcher <- _Server, 'Elixir.Demo.CalcServer':loop(_)]
    max(X.
      and(
        [_Server ? {_, {add, A, B}}] and(
          [_Server ! {add, Res} when Res =/= A + B and is_integer(A)]ff,
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

```erlang linenums='1' hl_lines='2 3 7'
fun(Var) -> receive Msg -> io:format(Msg) end end
fun() -> receive Msg -> io:format(Msg) end end
fun() -> receive Msg -> io:format(Msg) end end
fun() -> receive Msg -> io:format(Msg) end end
fun() -> receive Msg -> io:format(Msg) end end
fun() -> receive Msg -> io:format(Msg) end end
fun() -> receive Msg -> io:format(Msg) end end
```


The `#!python range()` function is used to generate a sequence of numbers.


++ctrl+alt+del++

++ctrl+v+tab+return+cmd++

!!! note
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla et euismod nulla. Curabitur feugiat, tortor non consequat finibus, justo purus auctor massa, nec semper lorem quam in massa.


???+ note "Phasellus posuere in sem ut cursus"
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla et euismod nulla. Curabitur feugiat, tortor non consequat finibus, justo purus auctor massa, nec semper lorem quam in massa.

???+ note
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla et euismod nulla. Curabitur feugiat, tortor non consequat finibus, justo purus auctor massa, nec semper lorem quam in massa.

    ``` python
    def bubble_sort(items):
        for i in range(len(items)):
            for j in range(len(items) - 1 - i):
                if items[j] > items[j + 1]:
                    items[j], items[j + 1] = items[j + 1], items[j]
    ```

    Nunc eu odio eleifend, blandit leo a, volutpat sapien. Phasellus posuere in
    sem ut cursus. Nullam sit amet tincidunt ipsum, sit amet elementum turpis.
    Etiam ipsum quam, mattis in purus vitae, lacinia fermentum enim.

!!! abstract "Hello"
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla et euismod nulla. Curabitur feugiat, tortor non consequat finibus, justo purus auctor massa, nec semper lorem quam in massa.


!!! example

    === "Unordered List"

        _Example_:
    
        ``` markdown
        * Sed sagittis eleifend rutrum
        * Donec vitae suscipit est
        * Nulla tempor lobortis orci
        ```
    
        _Result_:
    
        * Sed sagittis eleifend rutrum
        * Donec vitae suscipit est
        * Nulla tempor lobortis orci

    === "Ordered List"
    
        _Example_:
    
        ``` markdown
        1. Sed sagittis eleifend rutrum
        2. Donec vitae suscipit est
        3. Nulla tempor lobortis orci
        ```
    
        _Result_:
    
        1. Sed sagittis eleifend rutrum
    
        2. Donec vitae suscipit est
    
        3. Nulla tempor lobortis orci


The HTML specification is maintained by the W3C.

*[HTML]: Hyper Text Markup Language
*[W3C]: World Wide Web Consortium

**Duncan**

$$
\operatorname{ker} f=\{g\in {\color{sepia}{G}}:f(g)=e_{H}\}{\mbox{.}}
$$

The homomorphism $f$ is injective if and only if its kernel is only the
singleton set $e_G$, because otherwise $\exists a,b\in G$ with $a\neq b$ such
that $f(a)=f(b)$.