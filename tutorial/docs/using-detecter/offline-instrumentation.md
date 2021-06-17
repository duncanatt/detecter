--8<-- "includes/common.md"

# Offline Instrumentation
---

## Overview

The notion of outline instrumentation is extended to the offline case where the program potentially runs outside of the EVM.
detectEr implements a middleware component that emulates the EVM tracing infrastructure.
The enables detectEr to employ the *same* outline instrumentation algorithm we use in [Outline Instrumentation](outline-instrumentation.md) for offline instrumentation too.
Offline set-ups are generally the slowest in terms of verdict detection, by comparison to the inline and outline forms of instrumentation.
This stems from the dependence outline instrumentation has on the timely availability of pre-recorded runtime traces that are subject to external software entities, such as, files.

Our outline instrumentation middleware logic is encapsulated in the `#!erlang log_tracer` module that exposes a `#!erlang trace/1` function to dynamically attach to processes.
The `#!erlang log_tracer` module relies on log files at the medium through which programs can communicate trace events to the offline set-up.
detectEr can process plain text log files containing *complete* program runs, or *actively* monitor files for changes to dynamically dispatch events to analysers while the program is executing and updating the log file.
Of course, the log file in question must be known to both the program that writes to it and the `#!erlang log_tracer` module reading events.
Offline instrumentation supports the five events `fork`, `init`, `exit`, `send`, and `recv`, but assumes that event entries in log files are written in a pre-determined format recognisable by detectEr.
Events must be written *one per line*, structured as described in the table below.

| Program event  | Event entry pattern              | Entry variable     | Description                                                         |
| :------------: | :------------------------------- | :----------------: | :------------------------------------------------------------------ |
| `fork`         | `fork(P₁, P₂, {Mod, Fun, Args})` | `P₁`               | PID of the parent process spawning `P₂`                             |
|                |                                  | `P₂`               | PID of the child process spawned by `P₁`                            |
|                |                                  | `{Mod, Fun, Args}` | Function signature consisting of the module, function and arguments |
| `init`         | `init(P₂, P₁, {Mod, Fun, Args})` | `P₁`               | PID of the parent process spawning `P₂`                             |
|                |                                  | `P₂`               | PID of the child process spawned by `P₁`                            |
|                |                                  | `{Mod, Fun, Args}` | Function signature consisting of the module, function and arguments |
| `exit`         | `exit(P₁, Reason)`               | `P₁`               | PID of the terminated process                                       |
|                |                                  | `Reason`           | Termination reason                                                  |
| `send`         | `send(P₁, P₂, Msg)`              | `P₁`               | PID of the process issuing the message                              |
|                |                                  | `P₂`               | PID of the recipient process                                        |
|                |                                  | `Msg`              | Message payload                                                     |
| `recv`         | `recv(P₂, Msg)`                  | `P₂`               | PID of the recipient process                                        |
|                |                                  | `Msg`              | Message payload                                                     |

## The calculator server program

To demonstrate outline instrumentation, we once more use a *buggy* version of our calculator server that subtracts numbers, rather than adding them.
This Python implementation operates similar to the Erlang and Elixir analogues, but messages are exchanged in plain text over TCP sockets.
The buggy and correct server implementations may be found in the `examples/python` directory.

Both versions of this server expose the function `#!python start()` that accepts the argument `#!python n`.
`#!python start()` launches a thread that initialises and binds to a socket, and then repeatedly listens for incoming client connections on port `8080`.
This server loop mirrors the operation of our [Erlang](inline-instrumentation.md#the-calculator-server-program) and [Elixir](outline-instrumentation.md#the-calculator-server-program) implementations seen earlier.
Its communication protocol is, however, adapted slightly to suit the TCP setting.

In order for us to use offline instrumentation, the server needs to communicate its events externally.
This, it does by writing events using the standard Python logging facility in a user-specified log file.
The Python module provides a command line interface that launches the function `#!python start()`.
This CLI accepts two arguments, `n`, the request count to start the server with, and `log`, the file path of the log file where program events are written.

Launch a new terminal emulator window, navigate to the *root* detectEr directory, and:

1.  Change the directory to `example/python`:

    ```console
    [duncan@local]:/detecter$ cd examples/python
    [duncan@local]:/detecter/examples/python$ ls -l
    -rw-r--r--  1 duncan  duncan  1197 Jun 14 09:22 Makefile
    drwxr-xr-x  4 duncan  duncan   128 Jun 14 09:22 props
    drwxr-xr-x  3 duncan  duncan    96 Jun 14 09:22 src
    ```

2.  For the case of Python, we need to create a new virtual environment where all our dependencies can be installed in a sandbox that does not affect the global Python installation on your system.
    We do this via the `virtualenv` tool.
    If you have not already installed `virtualenv` for Python 3, follow the instructions in the [Installing the virtual environment management tool](../getting-started/preparation.md#installing-the-virtual-environment-management-tool) section.

    ```console
    [duncan@local]:/detecter/examples/python$ virtualenv venv
    created virtual environment CPython3.9.4.final.0-64 in 1230ms
    ...
    ```

3.  Next, we launch the virtual environment, change directory to `src`, and start the server.
    The server is started with a `#!python n` value of `#!python 0`, and set to write logs to the file `/detecter/examples/python/trace.log`.

    ```console
    [duncan@local]:/detecter/examples/python$ source venv/bin/activate
    [duncan@local]:/detecter/examples/python (venv)$ cd src
    [duncan@local]:/examples/python/src (venv)$ python -m demo.calc_server_bug 0 ../trace.log
    Started server on 0.0.0.0:8080..
    ```

    Now that that server is bound to port `8080` on `localhost`, it is ready to accept TCP requests.

4.  Communication with the server can be established via `telnet`.
    Launch a new terminal emulator window, and type:

    ```console
    [duncan@local]:/detecter/examples/python telnet localhost 8080
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    ```

    The server terminal window should indicate that a client is connected, displaying a message similar to 'Connected by 127.0.0.1:58576'.

5.  The calculator server communication protocol is text-based.
    Request operations start with a forward slash `/`, followed by the operation name (*e.g.*, `add`) and its arguments, separated by spaces.
    Every command is terminated with a carriage return.
    Let us try performing an addition request from the `telnet` CLI by typing:

    ```console
    /add 10 97
    -87.0
    ```

    We get the incorrect result of addition as expected.
    On the server terminal window, you should be able to see the output:

    ```console
    Received command /add 10.0 97.0.
    ```

6.  Having started the server and issued a first request, we are now in a position to examine the events that our Python server writes to `trace.log`.
    We can actively monitor this file from the terminal via the `tail` utility.
    Launch a third terminal emulator window, make sure you are in `examples/python`, and tail `trace.log`:

    ```console
    [duncan@local]:/detecter/examples/python tail -f trace.log
    fork(<0.102.0>,<0.808.0>,{calc_server_bug,loop,[0]})
    exit(<0.102.0>,normal)
    init(<0.808.0>,<0.102.0>,{calc_server_bug,loop,[0]})
    recv(<0.808.0>,{<0.585.0>,{add,10.0,97.0}})
    send(<0.808.0>,<0.585.0>,{ok,-87.0})
    ```

    These events describe the server operation up until this point.
    For instance, `fork(<0.102.0>,<0.808.0>,{calc_server_bug,loop,[0]})` indicates that the main thread that invokes the function `#!python start()` launches `#!python calc_server_bug.loop(n)` as a thread with `#!python n = 0`.
    Once this operation is completed, the main thread terminates, `exit(<0.102.0>,normal)`.
    Meanwhile, the calculator server thread exhibits the `init` event, subsequent to which, produces the `recv` and `send` program events following our `/add 10 97` request issued from the `telnet` CLI.
    Here, `<0.102.0>` is the PID of the main thread invoking `#!python start()`, and `<0.808.0>`, the PID of the server process.

7.  Keeping the `tail` window open, try issuing a multiplication request to the server:
    
    ```console
    /mul 10 97
    970.0
    ```

    You should see the recv and send events written to trace.log.

    ```console
    recv(<0.808.0>,{<0.585.0>,{mul,10.0,97.0}})
    send(<0.808.0>,<0.585.0>,{ok,970.0})
    ```

8.  We can stop the server by issuing a stop request using `telnet`:

    ```console
    /stp
    stopped
    Connection closed by foreign host.
    ```
    
    The corresponding server interaction and termination events are written to `trace.log`.

    ```console
    recv(<0.808.0>,{<0.585.0>,stp})
    send(<0.808.0>,<0.585.0>,{bye,0})
    exit(<0.808.0>,normal)
    ```

The correct implementation of the server in the `#!python calc_server` Python module can be launched following the steps above.

## Offline instrumentation in action

Since the sHML specifications in the script used for our inline instrumentation demo target the same function names `#!erlang calc_server_bug:loop/1` and `#!erlang calc_server:loop/1`, we *need not* regenerate the respective analysers.
Close `tail` by pressing ++ctrl+c++.
Create a new directory `ebin` inside `examples/python`, and copy the file `examples/erlang/ebin/prop_add_rec.beam` to it.

```console
[duncan@local]:/detecter/examples/python (venv)$ mkdir ebin
[duncan@local]:/detecter/examples/python (venv)$ cp ../erlang/ebin/prop_add_rec.beam ebin
[duncan@local]:/detecter/examples/python (venv)$ ls -l ebin
-rw-r--r--  1 duncan  duncan  2748 Jun 14 10:52 prop_add_rec.beam
```

To monitor log files, detectEr employs a concept similar to the `tail` utility we have just used.
Offline instrumentation is launched using `#!erlang module:start_offline/4`, that accepts four arguments:

1. the path of the log file where events are written (the log file need not necessarily exist; it will be automatically opened for parsing when created),

2. the PID of the root program process,

3. the function `#!erlang mfa_spec/1` of the analyser we want to use,

4. an option list.

The options supported by `#!erlang monitor:start_offline/4` are identical to [those](outline-instrumentation.md#outline-instrumentation-in-action) of `#!erlang monitor:start_online/3`.

Let us set up outline instrumentation.

1.  Make sure you are in the directory `/detecter/examples/python`.
    Delete `trace.log`:

    ```console
    [duncan@local]:/detecter/examples/python$ rm trace.log
    ```

2.  Launch a new Erlang shell, adding the detectEr binaries and the `ebin` directory containing the copied `prop_add_rec.beam`:

    ```console
    [duncan@local]:/detecter/examples/python$ erl -pa ../../detecter/ebin ebin
    Erlang/OTP 23 [erts-11.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]
    Eshell V11.2.1  (abort with ^G)
    1>
    ```

3.  Start offline instrumentation by invoking `#!erlang start_offline/4`.
    The first argument points to our yet-to-be-created `trace.log` file, the second is the PID of the root program process, third comes the analyser module, and finally, the list of options, which we leave empty.
    The root PID is required to bootstrap outline instrumentation.
    Recall that in our specific case, this is the PID of main thread that invokes the function `#!python start()`.
    We convert the PID manually from the constituent component parts `0`, `102`, `0` to an Erlang PID using the BIF `#!erlang pid/3`.

    ```erl
    1> monitor:start_offline("trace.log", pid(0,102,0), fun prop_add_rec:mfa_spec/1, []).
    init_it: Starter=<0.84.0>, Parent=<0.84.0>, Name={local,log_poller}, Mod=log_poller, Args=["trace.log"], Opts=[]
    <0.86.0>
    ```

4.  Launch the buggy calculator server from a separate terminal emulator window, as done previously.

    ```console
    [duncan@local]:/examples/python/src (venv)$ python -m demo.calc_server_bug 0 ../trace.log
    Started server on 0.0.0.0:8080..
    ```

    detectEr automatically registers the creation of `trace.log` and starts analysing events right away.
    It instruments the analyser and promptly analyses the first process event, `init`.
    This is translated to the internal Erlang `#!erlang spawned` event (see [From specification to analyser](synthesising-analysers.md#from-specification-to-analyser)).

    ```erl
    *** [<0.86.0>] Instrumenting monitor for MFA pattern '{calc_server_bug,loop,[0]}'.
    *** [<0.89.0>] Analyzing event {trace,<0.808.0>,spawned,<0.102.0>,{calc_server_bug,loop,[0]}}.
    ```

5.  Let us submit an addition request through `telnet`.

    ```
    [duncan@local]:/detecter/examples/python$ telnet localhost 8080
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    /add 10 97
    -87.0
    ```

    The Erlang shell hosting detectEr shows that this leads to the rejection verdict `no`, corresponding to a violation of property P~3~.

    ```erl hl_lines="3"
    *** [<0.89.0>] Analyzing event {trace,<0.808.0>,'receive',{<0.610.0>,{add,10.0,97.0}}}.
    *** [<0.89.0>] Analyzing event {trace,<0.808.0>,send,{ok,-87.0},<0.610.0>}.
    *** [<0.89.0>] Reached verdict 'no'.
    ```

6.  Stop the server:

    ```console
    /stp 
    stopped
    Connection closed by foreign host.
    ```

5. Stop the outline instrumentation. We need to do this since the log is read in a forward-only manner, and rewriting to it disrupts the parsing mechanism. 

    ```erl
    monitor:stop().
    ```

    The output on the Erlang shell is the same as the one we have [seen previously](outline-instrumentation.md#outline-instrumentation-in-action) with outline instrumentation, where analysers are terminated promptly upon verdict detection.


## Testing the correct server implementation

Go ahead and test the correct calculator server implementation, `#!python calc_server`, following the above steps.
Remember to remove `trace.log` prior to starting detectEr in offline mode, otherwise it will process the events from our previous run.
When executing `/add` requests, you should be able to see the offline analyser recurse via variable `#!shml X`.

```erl hl_lines="8"
1> monitor:start_offline("trace.log", pid(0,102,0), fun prop_add_rec:mfa_spec/1, []).
init_it: Starter=<0.84.0>, Parent=<0.84.0>, Name={local,log_poller}, Mod=log_poller, Args=["trace.log"], Opts=[]
<0.86.0>
*** [<0.86.0>] Instrumenting monitor for MFA pattern '{calc_server,loop,[0]}'.
*** [<0.89.0>] Analyzing event {trace,<0.808.0>,spawned,<0.102.0>,{calc_server,loop,[0]}}.
*** [<0.89.0>] Analyzing event {trace,<0.808.0>,'receive',{<0.612.0>,{add,10.0,97.0}}}.
*** [<0.89.0>] Analyzing event {trace,<0.808.0>,send,{ok,107.0},<0.612.0>}.
*** [<0.89.0>] Unfolding rec. var. 'X'.
```

Issuing both `/mul` and `/stp` requests leads to the inconclusive verdict `end`.

```erl
*** [<0.89.0>] Reached verdict 'end' on event {trace,<0.808.0>,'receive',{<0.612.0>,{mul,10.0,97.0}}}.
```

---
Congratulations on completing this tutorial! Woohoo!