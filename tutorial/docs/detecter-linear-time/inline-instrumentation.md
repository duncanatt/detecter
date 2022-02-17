--8<-- "includes/common.md"

# Inline Instrumentation
---

## Overview

Inline instrumentation for monitoring program traces works identically to the one in the [Inline Instrumentation](../using-detecter/inline-instrumentation.md) section, where the exact sequence of steps may be followed. 
The only difference is that now, instead of instrumenting the target program using the `#!erlang weaver` module, `#!erlang lin_weaver` should be used instead.
This module offers the same source code-level weaving functionality, and supports the same configuration options.
Refer to [Inline instrumentation in action](../using-detecter/inline-instrumentation.md#inline-instrumentation-in-action) for more details.

!!! note "Instrumentation Support"
    At present, the linear-time version of detectEr only supports inline instrumentation; outlining is currently under development, and will be added in the near future.

[comment]: <> (---)

[comment]: <> (The next instrumentation method, outlining, completely externalises the instrumentation and analysis, and does not assume access to the program source code, as inlining does.)



