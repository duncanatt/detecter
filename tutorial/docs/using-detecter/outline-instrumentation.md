--8<-- "includes/common.md"

# Outline Instrumentation
---

## Overview

Outline instrumentation is the non-invasive counterpart to inlining, where no changes are written to the program source code.
Outlining externalises the extraction of, and analysis of trace events.
It leverages the native tracing infrastructure the the EVM provides.
EVM tracing is used by a multitude of other programs and utilities that target the Erlang ecosystem, such as the debugger `dbg`.
The nice thing about Erlang tracing is that is supports any software component that is developed for the EVM.
For instance, languages like [Elixir](https://elixir-lang.org) and [Clojerl](https://www.clojerl.org) can benefit from our type of runtime monitoring.
Detailed information about the outline instrumentation algorithm detectEr uses can be found in the [companion paper](https://link.springer.com/content/pdf/10.1007%2F978-3-030-78089-0_14.pdf), and our [preprint](https://arxiv.org/abs/2104.09433).

## The calculator server program

For this demo, we will use an Elixir implementation of our *buggy* calculator server that subtracts numbers, rather than adding them.
This implementation, shown below, is identical to the one in Erlang in every aspect, except for the programming language syntax.
In particular, function invocations in Elixir may omit the parentheses (*e.g.*, `#!elixir send`, `#!elixir self`), and atoms are prepended with a colon (*e.g.*, `#!elixir :add`).
Elixir variables are specified in *lowercase*, as opposed to Erlang, which must be capitalised.
Finally, messages in Elixir are sent using the function `#!elixir send`, that accepts the PID of the recipient process and the message to be sent as arguments, and is the counterpart to `#!erlang !` in Erlang.

```elixir
def start(n) do
  spawn __MODULE__, :loop, [n]
end

def loop(tot) do
  receive do
    {clt, {:add, a, b}} ->

      # Handle addition request from client.
      send clt, {:ok, a - b} # Bug!!
      loop tot + 1

    {clt, {:mul, a, b}} ->

      # Handle multiplication request from client.
      send clt, {:ok, a * b}
      loop tot + 1

    {clt, :stp} ->

      # Handle stop request. Server does not loop again.
      send clt, {:bye, tot}
  end
end
```

The function `#!elixir Demo.CalcServerBug.start/1` returns the PID, as the case for the Erlang version, while the messages for addition, multiplication and termination requests are the same.

Let us familiarise ourselves with this implementation by executing it on the Elixir shell.
Launch a new terminal emulator window, navigate to the *root* detectEr directory, and:

1.  Change the directory to `example/elixir`.

    ```console
    [duncan@local]:/detecter$ cd examples/elixir
    [duncan@local]:/detecter/examples/elixir$ ls -l
    -rw-r--r--  1 duncan  duncan  4452 Jun 11 19:56 Makefile
    drwxr-xr-x@ 3 duncan  duncan    96 Apr 29 19:16 lib
    drwxr-xr-x@ 5 duncan  duncan   160 Jun 12 22:10 props
    ```

2.  Execute `make` to compile all of the Elixir source code modules located in `examples/elixir/lib`. 
    A new `ebin` directory containing the compiled `*.beam` files is created.

3.  Launch the Elixir shell `iex`.
    As we did in the Erlang case, we add the detectEr binaries and the ones we just compiled to the shell code path using `-pa`.
    By contrast to what we did when launching the Erlang shell, `-pa` must be prepend each directory we wish to add separately.

    ```iex
    [duncan@local]:/detecter/examples/elixir$ iex -pa ../../detecter/ebin -pa ebin
    Erlang/OTP 23 [erts-11.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]
    Interactive Elixir (1.11.4) - press Ctrl+C to exit (type h() ENTER for help)
    iex(1)>
    ```

4.  The buggy calculator server is started by invoking `#!elixir Demo.CalcServerBug.start/1` that accepts a single argument, `#!elixir n`, that it uses to track the number of requests handled.
    Like its Erlang counterpart `#!elixir Demo.CalcServerBug.start/1` returns the server process PID, which we assign to variable `#!elixir pid`.

    ```iex
    iex(1)> pid = Demo.CalcServerBug.start 0  
    #PID<0.109.0>
    ```

5.  The server is ready for client requests, which we submit using via `#!elixir send`.
    To add two numbers:

    {% raw %}
    ```iex
    iex(2)> send pid, {self, {:add, 10, 97}}
    {#PID<0.106.0>, {:add, 10, 97}}
    ```
    {% endraw %}

    Observe that the request we issued bears the same format as the one used in our Erlang implementation.
    Once again, note that {% raw %}`#!elixir {#PID<0.106.0>, {:add, 10, 97}}` {% endraw %} is not the server reply.
    Multiplying numbers is accomplished analogously.

6.  Viewing the server reply from the server is done using `#!elixir flush/0`.

    ```iex
    iex(3)> flush
    {:ok, -87}
    :ok
    ```

7.  To shutdown the server, we use `stp`.
    
    {% raw %}
    ```iex
    iex(4)> send pid, {self, :stp}
    {#PID<0.106.0>, :stp}
    iex(5)> flush
    {:bye, 1}
    :ok
    ```
    {% endraw %} 

The correct implementation of the server in the `#!elixir Demo.CalcServer` module can be launched by following the steps above.

## Outline instrumentation in action

We need to synthesise the analysers from the `prop_add_rec.hml` in `examples/elixir/props`.
These analysers designate the main calculator server loop functions `#!iex Demo.CalcServer.loop/1` and `#!iex Demo.CalcServerBug.loop/1` in their `#!shml with` statement.

```iex
iex(6)> :hml_eval.compile 'props/prop_add_rec.hml', [{:outdir, 'ebin'}, :v]
:ok
```

detectEr provides the `#!erlang monitor` module that enables us to instrument the program we want analysed using outlining.
This module offers two functions, `#!erlang start_online/3`, and `#!erlang start_offline/4` for conducting outline and offline instrumentation.
We use former function, and cover `#!erlang monitor:start_offline/4` in the next section on [Offline Instrumentation](offline-instrumentation.md).

`#!erlang start_online/3` accepts three arguments:

1. the tuple specifying the main function of the program we want instrumented in the format `#!erlang {Mod, Fun, Args}` where `#!erlang Mod` is the module name, `#!erlang Fun` is the main function to be launched, and `#!erlang Args`, the list of arguments accepted by `#!erlang Fun`,

2. the function `#!erlang mfa_spec/1` of the analyser we want to use, and,

3. an option list.

The options supported by `#!erlang start_online/3` are:

| Option | Description                                                                                                                                                                                                                                                                                                      |
| :----: | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `#!erlang parent`   | Supervisor process that is linked to tracers, or `#!erlang self` if no supervision is required. The supervisor is notified when tracers or analysers terminate their execution, via process linking. If left unspecified, defaults to `#!erlang self`.                                     |
| `#!erlang analysis` | Analysis mode, which can be either `#!erlang internal` or `#!erlang external`. Internal analysis means that the tracer process and analyser are one and the same; external analysis splits the tracing and analysers into separate processes. If left unspecified, defaults to `#!erlang internal`. |

`#!erlang start_online/3` returns the triple `#!erlang {ok, Pid, Return}`, consisting of the atom `#!erlang ok`, the PID of the root tracer `#!erlang Pid`, and the `#!erlang Return` result of the function specified in `#!erlang {Mod, Fun, Args}`.

Let us instrument the buggy calculator server, `#!elixir Demo.CalcServerBug`, to detect property violations.

1.  Launch the buggy server via `#!erlang start_online/3`. 
    As done above, we need to retain the PID of the calculator server process, which is the value returned by `#!elixir Demo.CalcServerBug.loop/1`. 
    We pattern match against the return value, and store the PID inside the variable `#!elixir pid`.
    `#!elixir Demo.CalcServerBug.loop/1` is specified as the tuple `#!elixir {Demo.CalcServerBug, :start, [0]}`, the first argument to `#!erlang start_online/3`.
    The second argument is the analyser function `#!elixir :prop_add_rec.mfa_spec/1` we synthesised earlier, and the list of options is left empty.
    
    ```iex
    iex(7)> {:ok, _, pid} = :monitor.start_online {Demo.CalcServerBug, :start, [0]}, &:prop_add_rec.mfa_spec/1, []
    *** [<0.109.0>] Instrumenting monitor for MFA pattern '{'Elixir.Demo.CalcServerBug',loop,[0]}'.
    *** [<0.111.0>] Analyzing event {trace,<0.110.0>,spawned,<0.108.0>,{'Elixir.Demo.CalcServerBug',loop,[0]}}.
    {:ok, #PID<0.109.0>, #PID<0.110.0>}
    ```

2.  As soon as the calculator server process starts, the analyser immediately enters into action and analyses the first process event `init`.
    This is translated to the internal Erlang `#!erlang spawned` event (see [From specification to analyser](synthesising-analysers.md#from-specification-to-analyser)).

3.  Now, let us try adding two numbers.

    {% raw %}
    ```iex hl_lines="4"
    iex(8)> send pid, {self, {:add, 10, 97}}
    *** [<0.111.0>] Analyzing event {trace,<0.110.0>,'receive',{<0.106.0>,{add,10,97}}}.
    *** [<0.111.0>] Analyzing event {trace,<0.110.0>,send,{ok,-87},<0.106.0>}.
    *** [<0.111.0>] Reached verdict 'no'.
    {#PID<0.106.0>, {:add, 10, 97}}
    ```
    {% endraw %}

    This leads to a rejection verdict `#!erlang no`, corresponding to a violation of our property P~3~.

4.  Stop the server. 

    {% raw %}
    ```iex
    iex(9)> send pid, {self, {:add, 10, 97}}
    {#PID<0.106.0>, {:add, 10, 97}}
    ```
    {% endraw %}

5. Stop the outline instrumentation.

    ```iex
    :monitor.stop.
    ```


## Irrevocable verdicts

By contrast to the [inline](inline-instrumentation.md) case, the `recv` and `send` events that were exhibited by the calculator server do not appear when the server terminates in step 4 above.
The question is why?
In outlining, analysers are *promptly* terminated when a verdict is flagged, in an effort to make things efficient.
This optimisation is possible because the verdicts analysers flag are irrevocable, so an analyser can terminate, safe in the knowledge that whatever verdict it has flagged, it will surely remain so.
As a byproduct of analyser termination, no event extraction from the program is possible, which is why the `recv` and `send` in step 4 are never observed.

## The program without analysis.

EVM tracing permits analysers to dynamically observe processes.
A terminated analyser or one that is never started means that no events from the program are extracted.
We can test this by launching the calculator server without going through `#!erlang monitor:start_online/3`.

{% raw %}
```iex
iex(10)> pid =  Demo.CalcServerBug.start 0
#PID<0.120.0>
iex(11)> send pid, {self, {:mul, 10, 97}}
{#PID<0.106.0>, {:mul, 10, 97}}
```
{% endraw %}

No analysers are instrumented, nor trace events extracted, since outlining never modifies the program.
Contrastingly, with inlining, a recompilation of the program is necessary should we want to run that program without instrumentation.
The flexibility due to outlining enables us to execute the program with no instrumentation by a mere restart without the need of redeployments.
In principle, analysers can also be attached while the program is operating, similar to how debuggers in Erlang work; this is an avenue left for near future work.

## Testing the correct server implementation

Try running the correct server `#!elixir Demo.CalcServer` as an exercise.
You need to start it as follows.

```iex
iex(12)> {:ok, _, pid} = :monitor.start_online {Demo.CalcServer, :start, [0]}, &:prop_add_rec.mfa_spec/1, []
```

---
The benefits that outlining offers can be taken a step further with offline instrumentation.