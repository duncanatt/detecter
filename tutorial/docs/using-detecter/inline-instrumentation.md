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
Here, we will learn how to employ detectEr to inline and analyse programs for which the source code *is* available.
Once more, we rely on our Erlang implementation of the calculator server from the [Getting Started](getting-started.md) section to show how this is done.

## The example system

Let us run the calculator server program with no instrumentation applied, to familiarise ourselves with the basic workflow.

make

launch the erlang shell

explain the protocol from the slides of section one

launch the server with Pid, isprocessalive

demonstrate addition, and flush

multiplication and flush

shutdown.
isprocessalive


Launch the Erlang shell, including the de


Mention the correct implementation

The incorrect implementation

Running the system

## Inlining in action

Show how the correct and incorrect operates.



