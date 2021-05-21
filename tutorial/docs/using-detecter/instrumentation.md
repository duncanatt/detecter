--8<-- "includes/common.md"

# Instrumentation
---

Types of instrumentation detecter supports.






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

We also need the updated parser.