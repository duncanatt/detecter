--8<-- "includes/common.md"

# Offline Instrumentation
---

## Overview

The notion of outline instrumentation is extended to the offline case where the program potentially runs outside of the EVM.
detectEr implements a middleware components that emulates the EVM tracing infrastructure.
The enables detectEr to employ the same outline instrumentation algorithm we use in [Outline Instrumentation](outline-instrumentation.md) for offline monitoring too.
Offline set-ups are generally the slowest in terms of verdict detection, by comparison to the inline and outline forms of instrumentation.
This stems from the dependence outline instrumentation has on the timely availability of pre-recorded runtime traces that are subject to external software entities, such as, files.

Our outline instrumentation middleware logic is encapsulated in the `#!erlang log_tracer` module that exposes a `#!erlang trace/1` function to dynamically attach to processes.
The `#!erlang log_tracer` module relies on log files at the medium through which programs can communicate trace events to the offline set-up.
detectEr can process plain text log files containing *complete* program runs, or *actively* monitor files for changes to dynamically dispatch events to analysers while the program is executing and updating the log file.
Of course, the log file in question must be known to both the program that writes to it and the `#!erlang log_tracer` module reading events.
Offline instrumentation supports the five events fork, init, exit, send, and recv, but assumes that event entries in the log files are written in a pre-determined format recognisable by detectEr.
Events must be written one per line, structured as described in the table below.

| Program event  | Log entry pattern                | Entry variable     | Description                                                         |
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
This Python implementation, operates similar to the Erlang and Elixir analogues, but messages are exchanged in plain text over TCP sockets.
The buggy and correct server implementations may be found in the `examples/python` directory.

The server is 

The calculator server relies on the standard Python logging facility to write trace events in a file specified.  


Interacting  with the server over TCP.

Starting

One add

Stop

Look at the trace file using the utility tail.



## Offline instrumentation in action

We can copy our synthesised analyser from the erlang or alternatively regenerate it from scratch. 

We use the copy to show that analysers are pluggable.








## Testing the correct server implementation