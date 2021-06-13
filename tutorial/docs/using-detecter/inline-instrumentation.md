--8<-- "includes/common.md"

# Inline Instrumentation
---

## Overview

Inlining is the most efficient instrumentation approach detectEr offers.
While it assumes access to the program source code, it carries benefits such as low runtime overhead and immediate detections.
There are various other RV tools, especially those targeting the JVM, that adopt inlining as their instrumentation method of choice, using libraries such as [AspectJ](https://www.eclipse.org/aspectj/) and [ASM](https://asm.ow2.io).
One drawback of inlining is that it tends to be invasive, and cannot be used in scenarios where the program code is not available (*e.g.* compiled versions of the program, licensing agreements, *etc.*).

detectEr employs a custom-built weaver to instrument invocations to analysers via code injection, by manipulating the program abstract syntax tree.
This procedure is detailed in the [companion paper](https://link.springer.com/content/pdf/10.1007%2F978-3-030-78089-0_14.pdf); readers are encouraged to consult this resource for more details.
Here, we will learn how to use detectEr to inline and analyse programs for which the source code *is* available.
Once more, we rely on our Erlang implementation of the calculator server from the [Getting Started](getting-started.md) section to show how this is done.

## The calculator server program

Let us run the calculator server program with no instrumentation applied, to familiarise ourselves with its basic operation.
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

4.  The calculator server is started by invoking `#!erlang calc_server:start/1`.
    If you recall from the section [Getting Started](getting-started.md), `#!erlang start/1` accepts a single argument `#!erlang N` that it uses to track the number of requests handled.
    Function `#!erlang start/1` spawns the server process and returns its PID.
    We need to hold on to this PID in order to use it to communicate with our server.
    To do this, we store the PID in the variable `#!erlang Pid`.

    ```erl
    1> Pid = calc_server:start(0).
    <0.86.0>
    ```

5.  We can check that the server process has actually started using the `is_process_alive/1` BIF.

    ```erl
    2> is_process_alive(Pid). 
    true
    ```

6.  The server is now ready to handle client requests.
    Messages in Erlang are submitted using the send operator, `#!erlang !`, that takes the PID of the recipient process and the message to send as arguments.
    The data in the request message we send must *coincide* with one of the clauses that the server is able to pattern match in its `#!erlang receive` expression.
    When this is not the case, the message is not processed, and left queued in the calculator server mailbox.
    Try instructing the server to add two numbers, `#!erlang 10` and `#!erlang 97`.

    ```erl
    3> Pid ! {self(), {add, 10, 97}}.
    {<0.84.0>,{add,10,97}}
    ```

    Let us dissect our request message.
    It consists of the pair containing the PID of the Erlang shell, which we obtain through the BIF `#!erlang self/0`, and the payload, `#!erlang {add, 10, 97}`.
    Send expressions in Erlang evaluate to the message sent, which is why the shell prints the output `#!erlang {<0.84.0>,{add,10,97}}`.
    Note that this is *not* the server reply.

7. To obtain the server reply from the mailbox of the shell, we use another BIF, `#!erlang flush/0`, that empties the mailbox contents on the shell.

    ```erl
    4> flush().
    Shell got {ok,107}
    ok
    ```

8.  Sending a multiplication request is similar to adding two numbers:

    ```erl
    5> Pid ! {self(), {mul, 10, 97}}.
    {<0.84.0>,{mul,10,97}}
    6> flush().
    Shell got {ok,970}
    ok
    ```

9.  To shutdown the server, we issue a `stp` request.
    
    ```erl
    7> Pid ! {self(), stp}.
    {<0.84.0>,stp}
    8> flush().
    Shell got {bye,2}
    ok
    9> is_process_alive(Pid).
    false
    ```

   Before terminating, our server replies to the sender with `#!erlang {bye,2}`, that contains the total number of requests serviced, `#!erlang add` and `#!erlang mul` in our example.
   The shell excerpt also confirms that the server process is terminated.
   We note that at this point, the server PID value `#!erlang <0.84.0>` is still instantiated in the variable Pid, which is now stale.
   Sending a message to a non-existing PID via `#!erlang !` is silently ignored without raising any errors.

Our buggy implementation of the server in the `#!erlang calc_server_bug` module operates exactly to the correct version, and can be launched by following the steps above.

## Inline instrumentation in action

Since in the previous section we executed `make` that cleans the `ebin` directory, we have to once more synthesise our analyser.

```erl
10> hml_eval:compile("props/prop_add_rec.hml", [{outdir, "ebin"}, v]).
ok
```

Now we can instrument the server we want analysed. 
detectEr provides the `#!erlang weaver` module that offers two functions, `#!erlang weave_file/3` that weaves a single file, and `#!erlang weave/3` that weaves an entire directory of files.
We will use `#!erlang weave/3` to weave both the correct and buggy implementation of our calculator server.
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


For this demo, we use the buggy version of our calculator server to detect property violations.

1.  Weave the `src/demo` directory by executing:

    ```erl
    11> weaver:weave("src/demo", fun prop_add_rec:mfa_spec/1, [{outdir, "ebin"}]).
    [{ok,calc_client,[]},
    {ok,calc_server,[]},
    {ok,calc_server_bug,[]},
    {ok,hello,[]}]
    ```

    The weaved files will be automatically loaded for you in the shell.

2.  Next, launch the buggy server.
    We will use the same variable `Pid` to keep hold of the PID returned by `calc_server_bug:start/1`.
    Since Erlang does not allow variables to be assigned more than once, we have to free the variable `Pid` before reusing it.
    We do this using the `f/1` BIF.

    ```erl
    12> f(Pid).
    ok
    13> Pid = calc_server_bug:start(0).
    *** [<0.84.0>] Instrumenting monitor for MFA pattern '{calc_server_bug,loop,[0]}'.
    *** [<0.141.0>] Analyzing event {trace,<0.141.0>,spawned,<0.84.0>,{calc_server_bug,loop,[0]}}.
    <0.141.0>
    ```

    As soon as the calculator server process starts, the analyser immediately enters into action and analyses the first process event `init`.
    Here we see this as the internal Erlang `#!erlang spawned` event (see [From specification to analyser](synthesising-analysers.md#from-specification-to-analyser)).

3.  Now, let us try an addition request.

    ```erl hl_lines="5"
    14> Pid ! {self(), {add, 10, 97}}.
    *** [<0.141.0>] Analyzing event {trace,<0.141.0>,'receive',{<0.84.0>,{add,10,97}}}.
    {<0.84.0>,{add,10,97}}
    *** [<0.141.0>] Analyzing event {trace,<0.141.0>,send,{ok,-87},<0.84.0>}.
    *** [<0.141.0>] Reached verdict 'no'.
    ```

    As expected, this leads to a rejection verdict, `#!erlang no`, that corresponds to a violation of our property P~3~.

4. Stop the server.

    ```erl hl_lines="2 3"
    15> Pid ! {self(), stp}.
    [INFO - <0.141.0> - analyzer:179] - Reached verdict 'no' after {recv,<0.141.0>,{<0.84.0>,stp}}.
    [INFO - <0.141.0> - analyzer:174] - Reached verdict 'no' after {send,<0.141.0>,<0.84.0>,{bye,1}}.
    {<0.84.0>,stp}
    ```

## Irrevocable verdicts

You might have noticed that the `recv` and `send` events exhibited by the calculator server process as it terminated (step 4 above) are also analysed by our analyser.
Both of these analyses yield the same rejection verdict of `no`.
Why does this happen?
In the section [Is one execution trace enough?](runtime-verification.md#is-one-execution-trace-enough) we explained that analysers yield irrevocable verdicts (*i.e.*, ones that cannot be retracted).
This is one such instance, where the verdict `no` is flagged, and the analyser will persist its decision, regardless of the events it analyses going forward.

## Testing the correct server implementation

Try running the correct version of the server, following steps 2-4 above.

1.  Launch the instrumented server.

    ```erl
    16> f(Pid).
    Pid = calc_server:start(0).
    *** [<0.84.0>] Instrumenting monitor for MFA pattern '{calc_server,loop,[0]}'.
    *** [<0.146.0>] Analyzing event {trace,<0.146.0>,spawned,<0.84.0>,{calc_server,loop,[0]}}.
    <0.146.0>
    ```

2.  Perform an addition request.

    ```erl hl_lines="5"
    17> Pid ! {self(), {add, 10, 97}}.
    *** [<0.146.0>] Analyzing event {trace,<0.146.0>,'receive',{<0.84.0>,{add,10,97}}}.
    {<0.84.0>,{add,10,97}}
    *** [<0.146.0>] Analyzing event {trace,<0.146.0>,send,{ok,107},<0.84.0>}.
    *** [<0.146.0>] Unfolding rec. var. 'X'.
    ```

   In this case where the addition is correctly executed by the server, the analyser unfolds the recursion via the variable `#!erlang X`, and resumes its analysis.

3.  Try submitting a multiplication request.

    ```erl
    18> Pid ! {self(), {mul, 10, 97}}.
    *** [<0.146.0>] Reached verdict 'end' on event {trace,<0.146.0>,'receive',{<0.84.0>,{mul,10,97}}}.
    {<0.84.0>,{mul,10,97}}
    ```

    The analyser reaches the inconclusive verdict `end` since given the event `#!erlang {trace,<0.146.0>,'receive',{<0.84.0>,{mul,10,97}}}`, it cannot determine whether these will eventually lead to a violation.
    Furthermore, now that the analyser has reached this point of inconclusiveness, it cannot backtrack, and will always yield the verdict `end` for every subsequent event it analyses.
    This is another instance of verdict irrevocability.

4.  Stopping the server elicits the same behaviour from the analyser.

    ```erl
    Pid ! {self(), stp}.
    [INFO - <0.146.0> - analyzer:179] - Reached verdict 'end' after {recv,<0.146.0>,{<0.84.0>,stp}}.
    [INFO - <0.146.0> - analyzer:174] - Reached verdict 'end' after {send,<0.146.0>,<0.84.0>,{bye,2}}.
    {<0.84.0>,stp}
    ```

---
The next instrumentation method, outlining, completely externalises the instrumentation and analysis, and does not assume access to the program source code, as inlining does.



