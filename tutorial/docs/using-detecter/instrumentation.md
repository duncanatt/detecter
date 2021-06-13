--8<-- "includes/common.md"

# Instrumentation
---

## Grey and black box approaches

Instrumentation is the second ingredient that lies at the heart of runtime monitoring.
It refers to the extraction of information from executing software, following one of two approaches.
In the *grey box* approach, instrumentation is implemented by *weaving* instructions that extract information, directly into the program under scrutiny.
Alternatively, instrumentation can leverage an existing tracing infrastructure to collect information externally.
In this manner, the program we want to observe is treated as a *black box*.
As you might have guessed, the kind of information that we want extracted from programs is the sequence of events, *i.e.*, the execution trace.
Besides extracting the trace, the instrumentation reports the events to the analysis that then processes them to reach the monitoring verdicts we introduced [previously](runtime-verification).

detectEr implements three instrumentation methods: *inline*, *outline*, and *offline* instrumentation.
Inline instrumentation, adopts the weaving approach, and is therefore the more invasive of the three.
Outlining makes use of the native tracing infrastructure provided by the EVM, whereas offline instrumentation emulates this tracing set-up via the use of log files that act as a trace storage that can be replayed.
detectEr preserves the separation of concerns that exists between the analysis and instrumentation.
Indeed, the analysers that we generate via the function `#!erlang hml_eval:compile/2` can be readily used with any of the three instrumentation approaches, unless of course, the targeted function spawning processes differs.

---
Now we detail how these may be used via three different implementations of our calculator server example, starting with the inline approach.






<!-- 


* Diagram of the system architecture.

* How the same generated analyzer code is used to instrument the Erlang, Elixir and Python setups. 



# Generating the monitor 

This is a common aspect to all types of instrumentation. Treating it centrally reinforces this fact.


## Inline instrumentation

We reintroduce the erlang system code.

The erroneous system here is on P1

## Outline instrumentation

We introduce the elixir system code.

The erroneous system here is on P2.

## Outline instrumentation

We introduce the python system code.

The erroneous system here is on P3.


---
As system implementations (for the tutorial), we should have:

1. The good system for all language implementations.

2. The system that violates P1 for just erlang.

3. The system that violates P2 for just elixir.

4. The system that violates P3 for just python, doris!

 -->
