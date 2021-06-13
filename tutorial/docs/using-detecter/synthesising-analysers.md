--8<-- "includes/common.md"

# Synthesising Analysers
---

## Trace events, in practice

When it comes to tooling, the representation of trace events tends to be implementation-specific and is often tied to the chosen language framework.
To disentangle the specification and analysis concepts, detectEr employs the *intermediate representation* seen [earlier](the-specification-logic.md#pattern-and-constraint-expressions), consisting of the program events, `fork`, `init`, `exit`, `send` and `recv`.
These intermediate events are internally translated to the ones that the native Erlang tracing infrastructure uses.
We adopt this approach since it simplifies the implementation of analysers. 

## From specification to analyser

To demonstrate how you can use detectEr to synthesise analysers, we use the formalisation of property P~3~ as a vehicle.
detectEr provides the `#!erlang hml_eval:compile/2` function that compiles sHML specifications to executable Erlang code.
The `#!erlang compile` function accepts two arguments, the path that points to the sHML script file, and a list of options that control how the resulting analyser is generated.
sHML script files are plain text formatted files with a `.hml` extension, and must at least contain one specification.
Multiple specifications are separated with comma, and every file must be terminated with a full-stop.
The configuration options that `#!erlang hml_eval:compile/2` are as follows.

| Option            | Description                                                                                                                                                           |
|:-----------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `#!erlang outdir` | Directory where the synthesised analyser file should be written. If left unspecified, defaults to the current directory `.`.                                          |
| `#!erlang v`      | Inserts logging statements into the synthesised output analyser file. Only use for debugging purposes.                                                                |
| `#!erlang erl`    | Instructs `#!erlang compile` function to output the synthesised analyser as Erlang source code rather than in `beam` format. If left unspecified, defaults to `beam`. |

Analysers are synthesised from the Erlang shell, which needs to have the detectEr binaries loaded in its code path.
This is done by launching the shell with the `-pa` flag.
For this example, we change the directory to `examples/erlang` and launch the Erlang shell, of course assuming that the detectEr source files have already been compiled.
Refer to the section [Setting up detectEr](../getting-started/setting-up-detecter.md) if you need to do this.

```console
[duncan@local]:$ cd examples/erlang
[duncan@local]:$ erl -pa ../../detecter/ebin
Erlang/OTP 23 [erts-11.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]
Eshell V11.2.1  (abort with ^G)
1>
```

With the Erlang shell loaded, compile the `prop_add_rec.hml`.

```erl
1> hml_eval:compile("props/prop_add_rec.hml", [{outdir, "ebin"}, erl]).
ok
```

We have specified the `#!erlang erl` option to generate the analyser source code in order to overview the internal workings of analysers.
You may skip this section if you want just learn how to to use the tool.
The analyser is written in the file `ebin/prop_add_rec.erl`, an excerpt of which is shown below.

```erlang linenums="1"
-module(prop_add_rec).
-author("detectEr").
-generated("2021/6/12 16:51:08").
-export([mfa_spec/1]).
mfa_spec({calc_server, loop, [_]}) ->
  {ok,
    fun({trace, _, spawned, _, {calc_server, loop, [_]}}) ->
      fun X() ->
        fun({trace, _, 'receive', {_, {add, A, B}}}) ->
          fun({trace, _, send, {ok, Res}, _}) when Res =/= A + B ->
            no;
            ({trace, _, send, {ok, Res}, _}) when Res =:= A + B ->
              X();
            (_) ->
              'end'
          end;
          (_) ->
            'end'
        end
      end();
      (_) ->
        'end'
    end};
    mfa_spec({calc_server_bug, loop, [_]}) ->
    ...
    ...
    end};
mfa_spec(_) ->
  undefined.
```

So the first thing that we should note is that the analyser is one higher-order function returns other functions or atoms in turn.
The `init`, `send`, `recv` event patterns in the specification are translated by `#!erlang hml_eval:compile/2` to the event format that the EVM uses for tracing.
These event mappings are tabled below.

| Program event  | Pattern                                | EVM trace event translation                           |
| :------------: | :------------------------------------: | :---------------------------------------------------: |
| `fork`         | `P₁` **-->** `P₂`, `Mod`:`Fun`(`Args`) | `#!erlang {trace, P₁, spawn, P₂, {Mod, Fun, Args}}`   |
| `init`         | `P₁` **<--** `P₂`, `Mod`:`Fun`(`Args`) | `#!erlang {trace, P₁, spawned, P₂, {Mod, Fun, Args}}` |
| `exit`         | `P₁` __**__ `Reason`                   | `#!erlang {trace, P₁, exit, Reason}`                  |
| `send`         | `P₁` **:** `P₂` **!** `Msg`            | `#!erlang {trace, P₁, send, Msg, P₂}`                 |
| `recv`         | `P₂` **?** `Msg`                       | `#!erlang {trace, P₁, 'receive', Msg}`                |

The entry point to analysers is the hardcoded function `#!erlang mfa_spec/1` that accepts as an argument the function that we designated with the keyword `#!shml with`, line `5`.
We revisit `#!erlang mfa_spec/1` when we discuss the instrumentation.
In the specific case of our calculator server example, the targeted function on line `5` is `#!erlang loop` in the `#!erlang calc_server` module.
Our examples packaged with the detectEr distribution include a second implementation of a buggy calculator server, `#!erlang calc_server_bug`, which we will use to show how analysers flag rejection verdicts.
`prop_add_rec.hml` specifies the same formalisation of P~3~ for this buggy version, the corresponding synthesised analyser code is given on lines `24`-`27` (omitted).
We will use this second analyser to show how a rejection verdict is reached when incorrect program behaviour is detected.

Our analysers behave similar to *state machines* that analyse events and transition to the next state that corresponds to the continuation formula.
In the analyser code above, the necessity with the `init` symbolic action, `#!shml and([_ <- _, calc_server:loop(_)]`, in our sHML formula corresponds to the function clause on line `7`.
Next comes the maximal fix-point construct that is translated to the named function `#!erlang X`, line `8`.
The name `#!erlang X` is used to recurse when the `send` event pattern matches to the correct reply consisting of the addition of the variables `#!erlang A` and `#!erlang B`.
Nested inside `#!erlang X` is the function on lines `9`-`19` that corresponds to the first necessity containing the receive process pattern `#!shml _ ? {_, {add, A, B}}`.
Finally, we have the analyser code for the two necessities that handle the incorrect and correct cases of addition, both enclosed in the parent function that handles `recv`.
This function consists of two clauses. 
The constraint `#!shml when Res =/= A + B` in the first clause is satisfied when the reply payload sent by the server contains anything other than the sum of `#!erlang A` and `#!erlang B`, leading to a rejection verdict `no` (line `11`).
The second clause on line `12` corresponds to the necessity `#!shml [_:_ ! {ok, Res} when Res =:= A + B]`, handling the case when the addition is correctly executed by the server.
Note that the body of the latter function is just the invocation of the outer function `#!erlang X`, which emulates looping via recursion.

The functions we discussed each have an extra 'catch all' clause that is inserted by the synthesiser (*e.g.*, lines `14`, `17`) to cater for the case where the event reported to the analyser is not the one expected.
This clause matches any other event, hence the use of the don't care pattern `_`.
In such instances, the analyser is unable to decide whether that event leads to a rejection verdict, *i.e.*, a violation of the property.
Not also that since the functions are nested withing each other, the variables in the outer functions bind the ones in the inner functions.
This *lexical scoping* is what allows us to refer to the variables `#!erlang A` and `#!erlang B` of the receive pattern `#!shml _ ? {_, {add, A, B}}` in the first necessity, from the constraints of the second necessities, `#!shml when Res =/= A + B` and `#!shml when Res =:= A + B`.




## Executable analysers

Now that we have seen what the analyser looks like from the inside, we can go ahead and recompile `prop_add_rec.hml` script, this time replacing the `#!erlang erl` option for `#!erlang v`.
Verbose analysers print the events they analyse on the Erlang shell.
Of course, the resulting analyser binary must be included in the code path of the program we are running, together with the detectEr binaries.
This will be instructive to help understand what happens when we instrument and runtime analyse the correct and buggy versions of our calculator server.

```erl
2> hml_eval:compile("props/prop_add_rec.hml", [{outdir, "ebin"}, v]).
ok
```

---
Now we look at the kinds of instrumentation methods detectEr offers.