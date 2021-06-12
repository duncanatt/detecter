
--8<-- "includes/common.md"

# Getting Started
<!-- # Concurrent programs -->
---

## A Calculator program in Erlang

Let us consider an archetypal calculator program consisting of a single server process that handles client requests for arithmetic computation.
The calculator server blocks and waits for requests sent as *asynchronous* messages.
These messages are addressed to the server using its PID, and deposited in the server mailbox that buffers multiple client requests.
The calculator server unblocks upon reading a message from its mailbox.
In our client-server protocol, messages contain the *type* of operation to be executed on the server side, its *arguments* (if applicable), and the client PID to whom the corresponding server reply is addressed.

Our calculator program is implemented as the Erlang module `#!erlang calc_server` that can be found under `examples/erlang/src`.

--8<-- "includes/calc_server.md"

`#!erlang calc_server:loop/1` encapsulates the calculator server logic that is [spawned](../getting-started/quickstart.md#hello-world-the-asynchronous-way) by some other launcher process (*e.g.* the Erlang shell) that invokes `#!erlang calc_server:start/1` for some integer argument `#!erlang N`.

The spawned server consumes a message request from its mailbox via the `#!erlang receive` expression on lines `5-16`, and pattern matches against *one* of the three types of operations requested by clients:

1. Addition (`#!erlang add`) and multiplication (`#!erlang mul`) requests that carry the operands `#!erlang A` and `#!erlang B` (lines `6` and `10`), and,

2. stop (`#!erlang stp`) requests that carries no arguments (line `14`).

Pattern matching instantiates the variables `#!erlang Clt`, `#!erlang A` and `#!erlang B` to *concrete data* in client request messages.
Every request fulfilled by the server results in a corresponding reply that it sends to the PID of the client instantiated in variable `#!erlang Clt`, lines `6`, `10`, and `14`.
Server replies carry the status *tag* (an atom) `#!erlang ok` or `#!erlang bye`, and the result of the requested operation.
The server uses the parameter `#!erlang Tot` of `#!erlang loop/1` to track the number of requests serviced, and is returned in reply to a `#!erlang stp` operation.
After handling `#!erlang add` and `#!erlang mul` requests, the server executes `#!erlang loop/1` to recommence the process loop and service the next client request, incrementing the request count accordingly (lines `8` and `12`); the tail recursive call to `#!erlang loop/1` is not made for `#!erlang stp` requests, and the calculator server process terminates naturally.

The logic of `#!erlang loop/1` induces a server runtime behaviour that can be *abstractly* described by the transition system model below.

--8<-- "includes/model.svg"
{: .center .img }

*States* of the model capture the internal state that the server process can be in at any point during its execution.
*Transitions* between states denote the computational steps of the program that produce *visible program events*.
For instance, the event `#!erlang Srv ? {Clt, stp}` is exhibited by the server loop when the calculator at its initial state `Q0` reads a `stp` request from its mailbox and transitions to `Q3`.
This transition depicts the computation that `#!erlang loop/1` performs to `#!erlang receive` the `#!erlang stp` request, line `5`, and subsequently pattern match it to `#!erlang {Clt, stp}` on line `14`.
Note that events in the model capture the *set of all possible concrete events* that the running program can exhibit, *e.g.*, `#!erlang Srv ? {Clt, stp}` describes all receive events where the *variable placeholders* `#!erlang Srv` and `#!erlang Clt` range over PIDs, and `#!erlang stp` is the atom denoting the stop operation requested by clients.

## Safety properties

There are a number of properties we would like the server behaviour to observe.
For example, the server loop does not control the initial value of `#!erlang Tot` that an invocation to `#!erlang calc_server:start/1` provides.
We could, therefore, require that:


> (P~1~)&nbsp;&nbsp;&nbsp;&nbsp;The service request count returned on shutdown is *never* negative.

Similarly, we would expect that

> (P~2~)&nbsp;&nbsp;&nbsp;&nbsp;Replies are *always* sent to the client indicated in the request,

and that 

> (P~3~)&nbsp;&nbsp;&nbsp;&nbsp;A request for adding two numbers *always* returns their sum

both hold, amongst others.
<!-- and "A request for adding two numbers *never* returns an incorrect sum" hold, amongst many others. -->
These properties are *data-dependent*, which makes them hard to ascertain using static techniques such as type systems.
Besides properties that reason about data, our server logic is expected to comply with control properties, such as,

> (P~4~)&nbsp;&nbsp;&nbsp;&nbsp;Client requests are *never* serviced more than once.

The properties mentioned thus far phrase the correctness requirement as a guarantee that the program *must always* provide.
Such properties are called *safety properties*, since they stipulate that "for *any* sort of behaviour that the program *can* do, nothing bad ever happens."
As a consequence of this condition, showing that a program *violates* a safety property entails finding just *one* instance of a program execution that exhibits bad behaviour.
Producing this evidence is enough of a proof that the program under scrutiny is not safe with respect to the property in question.

---
The next section explains how these properties can be expressed in a logic that *precisely and unambiguously* establishes the behaviour programs must comply with.