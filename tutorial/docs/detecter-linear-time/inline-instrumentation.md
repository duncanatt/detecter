--8<-- "includes/common.md"

# Inline Instrumentation
---

## Overview

Inline instrumentation for monitoring program traces works identically to the one in the [Inline Instrumentation](../using-detecter/inline-instrumentation.md) section, where the exact sequence of steps may be followed. 
The only difference is that now, instead of instrumenting the target program using the `#!erlang weaver` module, `#!erlang lin_weaver` should be used instead.
This module offers the same source code-level weaving functionality, and supports the same configuration options.
Refer to [Inline instrumentation in action](../using-detecter/inline-instrumentation.md#inline-instrumentation-in-action) for more details.

!!! note "Instrumentation Support"
    At present, the linear-time version of detectEr only supports inline instrumentation; outlining is currently under development, and will be added in the near future.

## The token server program

Let us run the token server program with no instrumentation applied, to familiarise ourselves with its basic operation.
Launch a new terminal emulator window, navigate to the *root* detectEr directory, and:

1.  Change the directory to `examples/erlang`.

    ```console
    [duncan@local]:/detecter$ cd examples/erlang
    [duncan@local]:/detecter/examples/erlang$ ls -l
    -rw-rw-r-- 1 duncan duncan  996 May 17 17:16 Makefile
    drwxrwxr-x 2 duncan duncan 4096 May 17 17:16 props
    drwxrwxr-x 3 duncan duncan 4096 May 17 17:16 src
    ```

2.  Execute `make` to compile all of the Erlang source code modules located in `examples/erlang/src`. 
    A new `ebin` directory containing the compiled `*.beam` files is created.

3.  Launch the Erlang shell `erl`.
    We add the detectEr binaries and the ones we have just compiled to the shell code path via `-pa`.

    ```console
    [duncan@local]:/detecter/examples/erlang$ erl -pa ../../detecter/ebin ebin
    Erlang/OTP 23 [erts-11.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]
    Eshell V11.2.1  (abort with ^G)
    1>
    ```

4.  The token server is started by invoking `#!erlang token_server:start/1`.
    If you recall from the section [Getting Started](getting-started.md), `#!erlang start/1` accepts a single argument `#!erlang Tok` that it uses to track the next identifier token.
    Function `#!erlang start/1` spawns the server process and returns its PID.
    We need to hold on to this PID in order to use it to communicate with our server.
    To do this, we store the PID in the variable `#!erlang Pid`.

    ```erl
    1> Pid = token_server:start(0).
    <0.86.0>
    ```

5.  We can check that the server process has actually started using the `is_process_alive/1` BIF.

    ```erl
    2> is_process_alive(Pid). 
    true
    ```

6.  The server is now ready to handle client requests.
    Messages in Erlang are submitted using the send operator, `#!erlang !`, that takes the PID of the recipient process and the message to send as arguments.
    
    The data in the request message we send must *coincide* with the clause that the server is able to pattern match in its `#!erlang receive` expression.
    When this is not the case, the message is not processed, and left queued in the token server mailbox.
    Try requesting a new identifier token.
    
    ```erl
    3> Pid ! {self(), 0}.
    (1) Serving token request with 1.
    {<0.84.0>,0}
    ```

    <!-- Let us dissect our request message. -->
    <!-- It consists of the pair containing the PID of the Erlang shell, which we obtain through the BIF `#!erlang self/0`, and the payload, `#!erlang {add, 10, 97}`. -->
    Send expressions in Erlang evaluate to the message sent, which is why the shell prints the output `#!erlang {<0.84.0>,0}`.
    Note that this is *not* the server reply.

7. To obtain the server reply from the mailbox of the shell, we use another BIF, `#!erlang flush/0`, that empties the mailbox contents on the shell.

    ```erl
    4> flush().
    Shell got {ok,1}
    ok
    ```

8.  To interrupt the Erlang shell, press ++ctrl+c++ followed by ++a++.
    
    ```erl
    5> 
    BREAK: (a)bort (A)bort with dump (c)ontinue (p)roc info (i)nfo
       (l)oaded (v)ersion (k)ill (D)b-tables (d)istribution
    ```

   We note that at this point, the server PID value `#!erlang <0.84.0>` is still instantiated in the variable Pid, which is now stale.
   Sending a message to a non-existing PID via `#!erlang !` is silently ignored without raising any errors.

## Inline instrumentation in action

Since in the previous section we executed `make` that cleans the `ebin` directory, we have to once more synthesise our analyser.

```erl
6> maxhml_eval:compile("props/prop_no_leak.hml", [{outdir, "ebin"}]).
ok
```

Now we can instrument the server we want analysed. 
detectEr provides the `#!erlang lin_weaver` module that offers two functions, `#!erlang weave_file/3` that weaves a single file, and `#!erlang weave/3` that weaves an entire directory of files.
We will use `#!erlang weave/3` to weave the directory where our token server is located.
The two variants of `#!erlang weave` accept three arguments: 

1. the file where the Erlang source file to be weaved resides (or directory of files, in case of `#!erlang weave/3`),

2. the function `#!erlang mfa_spec/1` of the analyser we want weaved, and,

3. an option list.

The options supported by `#!erlang weave_file/3` and `#!erlang weaver/3` are as follows:


| Option            | Description                                                                                                                             |
| :---------------: | :-------------------------------------------------------------------------------------------------------------------------------------- |
| `#!erlang outdir` | Directory where the generated weaved files should be written. If left unspecified, defaults to the current directory `.`.               |
| `#!erlang i`      | Directory containing include files that the source files in the source directory depend on.                                             |
| `#!erlang filter` | Filter function that suppresses events. If left unspecified, defaults to 'allows any'.                                                  |
| `#!erlang erl`    | Instructs the compiler to output the generated files as Erlang source code rather than `beam`. If left unspecified, defaults to `beam`. |


For this demo, we use our token server to detect property violations.

1.  Weave the `src/demo` directory by executing:

    ```erl
    7> lin_weaver:weave("src/demo", fun prop_no_leak:mfa_spec/1, [{outdir, "ebin"}]).
    [{ok,calc_client,[]},
    {ok,calc_server,[]},
    {ok,token_server,[]}]
    {ok,calc_server_bug,[]},
    {ok,hello,[]}    
    ```

    The weaved files will be automatically loaded for you in the shell.

2.  Next, launch the server.
    We will use the same variable `Pid` to keep hold of the PID returned by `token_server:start/1`.
    Since Erlang does not allow variables to be assigned more than once, we have to free the variable `Pid` before reusing it.
    We do this using the `f/1` BIF.

    ```erl
    8> f(Pid).
    ok
    9> Pid = token_server:start(1).    
    <0.88.0>
    ```

    As soon as the token server process starts, the analyser immediately enters into action and analyses the first process event `init`.
    <!-- Here we see this as the internal Erlang `#!erlang spawned` event (see [From specification to analyser](synthesising-analysers.md#from-specification-to-analyser)). -->

3.  Now, let us request a token.

    ```erl hl_lines="5"
    10> Pid ! {self(), 0}}.
    {<0.84.0>,1}
    :: Violation: Reached after analyzing event {send,<0.88.0>,<0.84.0>,1}.
    ```

    This leads to a rejection verdict, `#!erlang no`, that corresponds to a violation of our property P~3~.

4. Try requesting a second token.

    ```erl hl_lines="2 3"
    11> Pid ! {self(), 0}}.
    {<0.84.0>,0}
    :: Violation: Reached after analyzing event {recv,<0.88.0>,{<0.84.0>,0}}.
    :: Violation: Reached after analyzing event {send,<0.88.0>,<0.84.0>,2}.
    ```

## Irrevocable verdicts

You might have noticed that the `recv` and `send` events exhibited by the server process as it terminated (step 4 above) are also analysed by our analyser.
Both of these analyses yield the same rejection verdict of `no`.
Why does this happen?
In the section [Is one execution trace enough?](../using-detecter/runtime-verification.md#is-one-execution-trace-enough) we explained that analysers yield irrevocable verdicts (*i.e.*, ones that cannot be retracted).
This is one such instance, where the verdict `no` is flagged, and the analyser will persist its decision, regardless of the events it analyses going forward.

## Correcting the server implementation

Let us once again look at the server implementation.

--8<-- "includes/token_server.md"

Now we know that our implementation contains a bug that is exhibited as soon as the token server issues its first token.
A close at the server that the main server loop `#!erlang token_server:loop/2` is launched with the value of `#!erlang Tok` on line `2`.
This same value is then returned in reply to a client request.
There are two possible solutions to this bug.
The first solution is to start the server loop, `#!erlang token_server:loop/2`, with a different next token offset that will surely not be returned by the server.
One good candidate (amongst infinitely many others) is to use the arguments `#!erlang (Tok, Tok + 1)` on line `2`.
The second solution is to change the code on line `7` to `#!erlang Clt ! NextTok + 1`.

Try running the correct version of the server, following steps 2-4 above.

1.  Launch the instrumented server.

    ```erl
    12> f(Pid).
    Pid = token_server:start(1).
    <0.90.0>
    ```

2.  Perform an token request.

    ```erl hl_lines="5"
    13> Pid ! {self(), 0}.
    {<0.84.0>,0}    
    ```

   In this case where the request is correctly executed by the server, the analyser unfolds the recursion via the variable `#!erlang X`, and resumes its analysis.

3.  Try requesting a second token.

    ```erl
    14> Pid ! {self(), 0}.
    {<0.84.0>,0}
    ```

    The analyser reaches the inconclusive verdict `end` since given the event `#!erlang {trace,<0.146.0>,'receive',{<0.84.0>,{mul,10,97}}}`, it cannot determine whether these will eventually lead to a violation.
    Furthermore, now that the analyser has reached this point of inconclusiveness, it cannot backtrack, and will always yield the verdict `end` for every subsequent event it analyses.
    This is another instance of verdict irrevocability.

The runtime analysis does not detect any violations of the property.

