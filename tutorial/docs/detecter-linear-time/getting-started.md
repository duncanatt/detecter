
--8<-- "includes/common.md"

# Getting Started
---

## A Token Server Program in Erlang

Let us consider a token server program consisting of a single process that handles client requests for obtaining integer identifier tokens.
The server blocks and waits for requests sent as *asynchronous* messages.
These messages are addressed to the server using its PID, and deposited in the server mailbox that buffers multiple client requests.
The token server unblocks upon reading a message from its mailbox.
In our client-server protocol, messages contain the command `#!erlang 0`, signifying a new token request, and the client PID to whom the corresponding server reply should be addressed.

Our token server program is implemented as the Erlang module `#!erlang token_server` that can be found under `examples/erlang/src`.

--8<-- "includes/token_server.md"

`#!erlang token_server:loop/1` encapsulates the token server logic that is [spawned](../getting-started/quickstart.md#hello-world-the-asynchronous-way) by some other launcher process (*e.g.* the Erlang shell) that invokes `#!erlang token_server:start/1` for some integer argument `#!erlang Tok`.
This argument `#!erlang Tok` is the private server token that should *never* be leaked.

The spawned server consumes a message request from its mailbox via the `#!erlang receive` expression on lines `5-9`, and pattern matches against the new token request operation `#!erlang 0`.

Every request fulfilled by the server results in a corresponding reply that it sends to the PID of the client instantiated in variable `#!erlang Clt`, line `6`.
The server uses the parameter `#!erlang NextTok` of `#!erlang loop/2` to track the current token number; `#!erlang OwnTok` contains the private token of the token server itself.
After handling a request `#!erlang 0`, the server executes `#!erlang loop/2` to recommence the process loop and service the next client request, incrementing the next token value accordingly (line `8`).
Note that our server is designed *not* to terminate.

<!-- The logic of `#!erlang loop/1` induces a server runtime behaviour that can be *abstractly* described by the transition system model below. -->

<!-- --8<-- "includes/model.svg" -->
<!-- {: .center .img } -->

<!-- *States* of the model capture the internal state that the server process can be in at any point during its execution.
*Transitions* between states denote the computational steps of the program that produce *visible program events*.
For instance, the event `#!erlang Srv ? {Clt, stp}` is exhibited by the server loop when the calculator at its initial state `Q0` reads a `stp` request from its mailbox and transitions to `Q3`.
This transition depicts the computation that `#!erlang loop/1` performs to `#!erlang receive` the `#!erlang stp` request, line `5`, and subsequently pattern match it to `#!erlang {Clt, stp}` on line `14`.
Note that events in the model capture the *set of all possible concrete events* that the running program can exhibit, *e.g.*, `#!erlang Srv ? {Clt, stp}` describes all receive events where the *variable placeholders* `#!erlang Srv` and `#!erlang Clt` range over PIDs, and `#!erlang stp` is the atom denoting the stop operation requested by clients. -->

## Safety properties

There are a number of properties we would like the server execution to observe.
For example, the server loop does not control the initial value of `#!erlang Tok` that an invocation to `#!erlang token_server:start/1` provides.
We could, therefore, require that:


> (P~1~)&nbsp;&nbsp;&nbsp;&nbsp;Failure *does not* occur when the server is stated with its identifier token.

Similarly, we would expect that

> (P~2~)&nbsp;&nbsp;&nbsp;&nbsp;The server starts with its *correct* identifier token of `#!erlang 1`,

and that 

> (P~3~)&nbsp;&nbsp;&nbsp;&nbsp;The token `#!erlang 1` is *not* leaked by the server in reply to a client request.

both hold.
These properties are *data-dependent*, which makes them hard to ascertain using static techniques such as type systems.

The properties mentioned thus far phrase the correctness requirement as a guarantee that the program *must always* provide.
Such properties are called *safety properties*, since they stipulate that "for *any* sort of behaviour that the program *can* do, nothing bad ever happens."
As a consequence of this condition, showing that a program *violates* a safety property entails finding just *one* instance of a program execution that exhibits bad behaviour.
Producing this evidence is enough of a proof that the program under scrutiny is not safe with respect to the property in question.

---
The next section explains how these properties can be expressed in a logic that *precisely and unambiguously* establishes the behaviour program executions must comply with.