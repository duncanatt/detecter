--8<-- "includes/common.md"

# The Specification Logic maxHML
---

## Overview

maxHML enables us to express properties on the executions of programs.
Like sHML, maxHML is a another syntactic subset of the more expressive Hennessy-Milner Logic with recursion.
It is used to express safety properties, but by contrast to the sHML fragment of [The Specification Logic sHML](../using-detecter/the-specification-logic.md), maxHML is interpreted over *executions* (called *traces*), rather than process execution graphs.
maxHML formulae are generated from the following grammar:

```{ .maxhml .annotate }
φ, ψ ∈ maxHML ::=  ff  |  tt             (1)
       |  <α>φ                           (2)
       |  [α]φ                           (3)
       |  φ or ψ                         (4)
       |  φ and ψ                        (5)
       |  X                              (6)
       |  max(X. φ)                      (7)
```

1. Formulae `#!maxhml ff` and `#!maxhml tt` denote *falsity* and *truth* respectively,

2. `#!maxhml <α>φ` is the existential modal operator guarding the continuation formula `#!maxhml φ` (also called a *possibility*),

3. `#!maxhml [α]φ`, the dual of `#! <α>φ`, is the universal modal operator guarding the continuation formula `#!maxhml φ` (also called a *necessity*),

4. `#!maxhml φ or ψ` is a *disjunction*,

5. `#!maxhml φ and ψ` is a *conjunction*,

6. `#!maxhml X` is a *logical variable*, and,
   
7. `#!maxhml max(X. φ)` is the *greatest fixed point* construct that specifies *recursion* via the logical variable `#!maxhml X` and *binds* the free occurrences of `#!maxhml X` in the sub-formula `#!maxhml φ`. 

To handle reasoning over program event data, the modal operator is equipped with *symbolic actions* `#!maxhml α` of the form `#!maxhml P when C`, where `#!maxhml P` is an *event pattern* and `#!maxhml C`, a *decidable Boolean constraint*.
Patterns correspond to events that the program under analysis exhibits. 
These patterns contain *data variables* that are instantiated with values learnt at runtime from *matched* events.
Pattern variables *bind* the free variables in constraints `#!maxhml C`, and this binding scope *extends* to the continuation formula `#!maxhml φ`.
Symbolic action patterns follow the pattern-matching syntax of Erlang and Elixir, where atoms are matched directly, and the 'don't care' pattern `_` matches any data value.


We say that a trace satisfies the formula `#!maxhml <P when C>φ` *when* it exhibits an event that matches pattern `#!maxhml P`, fulfils the constraint `#!maxhml C`, *and* the ensuing program behaviour *also* satisfies `#!maxhml φ`.
Dually, a trace satisfies `#!maxhml [P when C]φ` *whenever* it exhibits an event that matches pattern `#!maxhml P`, fulfils the constraint `#!maxhml C`, *and* the ensuing program behaviour *then* satisfies `#!maxhml φ`.
When the constraint is `#!maxhml true`, the expression `#!maxhml when C` may be omitted for readability.

## Pattern and constraint expressions

detectEr supports five event patterns describing the lifecycle of processes.
A `fork` action is exhibited by a process when it spawns a new child process; its dual, `init`, is exhibited by the corresponding child upon initialisation.
Process `exit` actions signal termination, while `send` and `recv` describe process interaction.

| Program event  | Event pattern                             | Pattern variable    | Description                                                         |
| :-----------: | :----------------------------------------: | :-----------------: | :------------------------------------------------------------------ |
| `fork`        | `P₁` **-->** `P₂`, `Mod`:`Fun`(`Args`)     | `P₁`                | PID of the parent process spawning `P₂`                             |
|               |                                            | `P₂`                | PID of the child process spawned by `P₁`                            |
|               |                                            | `Mod`:`Fun`(`Args`) | Function signature consisting of the module, function and arguments |
| `init`        | `P₁` **<--** `P₂`, `Mod`:`Fun`(`Args`)     | `P₁`                | PID of the parent process spawning `P₂`                             |
|               |                                            | `P₂`                | PID of the child process spawned by `P₁`                            |
|               |                                            | `Mod`:`Fun`(`Args`) | Function signature consisting of the module, function and arguments |
| `exit`        | `P₁` __*__ `Reason`                        | `P₁`                | PID of the terminated process                                       |
|               |                                            | `Reason`            | Termination reason                                                  |
| `send`        | `P₁` **:** `P₂` **!** `Msg`                | `P₁`                | PID of the process issuing the message                              |
|               |                                            | `P₂`                | PID of the recipient process                                        |
|               |                                            | `Msg`               | Message payload                                                     |
| `recv`        | `P₂` **?** `Msg`                           | `P₂`                | PID of the recipient process                                        |
|               |                                            | `Msg`               | Message payload                                                     |

The variables `P₁` and `P₂` in event patterns must be a port ID or PID, whereas `Reason` and `Msg` may be any [Erlang data type](http://erlang.org/documentation/doc-6.0/doc/reference_manual/data_types.html), *i.e.*, one of atom, Boolean, integer, float, string, bit string, reference, fun, port ID, PID, tuple, map, and list.
`Mod` and `Fun` must be atoms, and `Args`, an arbitrary list comprised of the aforementioned data types.

!!! note "Pattern matching"
    Our current detectEr syntax does not yet implement full Erlang pattern matching, including $, map, record and bit string expressions; these will be added in future releases of the tool.
    Note that these data values can still be used in patterns, so long as the pattern matching expression does not *unwrap* the individual data components of these values.
    For instance, the pattern `Var = Map` is acceptable whereas `#{K := V} = Map` is not; similarly, `Var = List` may be used but not `[$d, $a | _] = List`.
    
Constraint definitions on pattern variables used by detectEr correspond to Erlang [guard sequences](https://erlang.org/doc/reference_manual/expressions.html#guard-sequences) consisting of [guard expressions](https://erlang.org/doc/reference_manual/expressions.html#guard-sequences).
The set of valid Erlang guards supported by detectEr are the following:

* Variables.
* Values, *i.e.*, atom, Boolean, integer, float, string, bit string, reference, fun, port ID, PID, tuple, map, and list.
* Expressions constructing atoms, integer, floats, lists, and tuples.



* Term comparisons.

    | Operator       | Description              |
    | :------------: | :----------------------- |
    | `#!erlang ==`  | Equal to                 |
    | `#!erlang /=`  | Not equal to             |
    | `#!erlang =<`  | Less than or equal to    |
    | `#!erlang <`   | Less than                |
    | `#!erlang >=`  | Greater than or equal to |
    | `#!erlang >`   | Greater than             |
    | `#!erlang =:=` | Exactly equal to         |
    | `#!erlang =/=` | Exactly not equal to     |

* Arithmetic expressions.

    | Operator        | Description               | Argument Type |
    | :-------------: | :------------------------ | :-----------: |
    | `#!erlang +`    | Unary addition            | Number        |
    | `#!erlang -`    | Unary subtraction         | Number        |
    | `#!erlang +`    | Addition                  | Number        |
    | `#!erlang -`    | Subtraction               | Number        |
    | `#!erlang *`    | Multiplication            | Number        |
    | `#!erlang /`    | Floating point division   | Number        |
    | `#!erlang bnot` | Unary bitwise NOT	        | Integer       |
    | `#!erlang div`  | Integer division          | Integer       |
    | `#!erlang rem`  | Integer remainder of X/Y  | Integer       |
    | `#!erlang band` | Bitwise AND               | Integer       |
    | `#!erlang bor`  | Bitwise OR                | Integer       |
    | `#!erlang bxor` | Arithmetic bitwise XOR    | Integer       |
    | `#!erlang bsl`  | Arithmetic bit shift left | Integer       |
    | `#!erlang bsr`  | Bit shift right           | Integer       |

* Boolean expressions.

    | Operator       | Description       |
    | :------------: | :---------------: |
    | `#!erlang not` | Unary logical NOT |
    | `#!erlang and` | Logical AND       |
    | `#!erlang or`  | Logical OR        |
    | `#!erlang xor` | Logical XOR       |

* Short-circuit expressions `#!erlang andalso`, `#!erlang orelse`.


## A simple example

Let us try to specify a safety requirement on the *traces* of our [token server program](getting-started.md#A-token-server-program-in-erlang).
<!-- The maxHML formula with symbolic action `#!maxhml Srv:Clt ! {bye, Tot} when Tot < 0` describes the property requiring that "the trace exhibited by the program does *not* contain a send event whose payload consists of `#!erlang {bye, Tot}` with a negative total: -->
The maxHML formula with symbolic action `#!maxhml Prnt ← Srv, token_server:loop([OwnTok, _]) when OwnTok =/= 1` describes the property requiring that "the server does not start with an identifier token other than `#!erlang 1`".

```maxhml
[Prnt ← Srv, token_server:loop([Tok]) when Tok =/= 1]ff
```

Recall that the universal modality states that, for any program event satisfying the symbolic action `#!maxhml P when C` in `#!maxhml [P when C]φ`, the rest of the trace must then satisfy the continuation formula `#!maxhml φ`.
However, *no* trace continuation can satisfy the formula `#!maxhml ff`! 
This means that the `#!maxhml [Prnt ← Srv, token_server:loop([OwnTok, _]) when OwnTok =/= 1]ff` can only be satisfied when our trace does not exhibit the event described by the symbolic action `#!erlang Prnt ← Srv, token_server:loop([OwnTok, _]) when OwnTok =/= 1`.

Suppose our server with PID `#!erlang <0.10.0>` exhibits the `init` event `#!erlang <0.16.0> ← <0.10.0>, token_server:loop([-1, 0])` when launched by a parent process with PID `#!erlang <0.16.0>`.
It matches pattern `#!maxhml Prnt ← Srv, token_server:loop([OwnTok, _]) when OwnTok =/= 1`, instantiating the variables `#!erlang Srv = <0.10.0>`, `#!erlang Prnt = <0.16.0>`, and `#!erlang OwnTok = -1`.
The constraint `#!erlang when OwnTok =/= 1` is also satisfied, leading to a violation, *i.e.*, `#!maxhml ff`.

For a different `init` event `#!erlang <0.16.0> ← <0.10.0>, token_server:loop([1, 0])`, the symbolic action in the modality `#!maxhml [Prnt ← Srv, token_server:loop([OwnTok, _]) when OwnTok =/= 1]` is not satisfied, and consequently, `#!maxhml [Prnt ← Srv, token_server:loop([OwnTok, _]) when OwnTok =/= 1]ff` is satisfied.
Analogously, the `init` event `<0.16.0> ← <0.10.0>, calc_server:loop()` satisfies the formula since the pattern `#!maxhml Prnt ← Srv, token_server:loop([Tok])` fails to match the event shape.


<!-- Suppose our server with PID `#!erlang <0.10.0>` exhibits the `init` event `#!erlang <0.10.0>:<0.16.0> ! {bye, -1}` in response to a request issued by a client with PID `#!erlang <0.16.0>`.
It matches pattern `#!maxhml Srv:Clt ! {bye, Tot}`, instantiating the variables `#!erlang Srv = <0.10.0>`, `#!erlang Clt = <0.16.0>`, and `#!erlang Tot = -1`.
The constraint `#!erlang when Tot < 0` is also satisfied by `#!erlang Tot`, leading to a violation, *i.e.*, `#!maxhml ff`.
For a different `send` event `#!erlang <0.10.0>:<0.16.0> ! {bye, 1}`, the symbolic action in the modality `#!maxhml [Srv:Clt ! {bye, Tot} when Tot < 0]` is not satisfied, and consequently, `#!maxhml [Srv:Clt ! {bye, Tot} when Tot < 0]ff` is not violated.
Analogously, the `exit` event `#!erlang <0.10.0> ** killed` does not lead to a violation of the formula since the pattern `#!maxhml Srv:Clt ! {bye, Tot}` fails to match the event shape. -->

!!! note "Cheat sheet"
    The formula `#!maxhml <α>tt` means that it is possible for the trace to exhibit the symbolic action `#!maxshml α`. Dually, `#!maxhml [α]ff` states that the trace must not exhibit the symbolic action `#!maxhml α`.

You should try expressing the safety properties P~1~, P~2~, and P~3~ stated informally in the [Getting Started](getting-started.md) section in terms of maxHML.
Keep in mind that now, our formal model is the *set of traces generated by the program*, rather than the program transition graph itself.

---
We next learn how runtime monitors can be synthesised from maxHML.