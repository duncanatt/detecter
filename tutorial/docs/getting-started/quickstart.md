--8<-- "includes/common.md"

# Quickstart
---

## Using detectEr from the shell

The `detecter/ebin` directory created [previously](setting-up-detecter.md) needs to be included in the Erlang runtime path to preload all the binaries required to be able to use detectEr.
This is done by using the `-pa` switch when launching the Erlang shell.

```
erl -pa /path/to/detecter/ebin
```

Once loaded, detectEr can be used to monitor any system that executes on the Erlang Virtual Machine.
It also supports the monitoring of systems that run outside of the EVM, providing these record their logs in files whose every log entry follows a predetermined formatting that enables detectEr to extract runtime information.
This topic is covered in more depth in the [Instrumentation](../using-detecter/instrumentation.md) section.
For now, we establish the workflow required to use detectEr by way of the traditional hello world example.


## Hello world

Before using detectEr, you should get an intuitive grasp of what monitoring involves, and the steps one needs to follow to initialise and start the monitoring process.
This also helps you to get acquainted with the Erlang shell if this is your first time using Erlang.
Launch a new terminal emulator window (*e.g.* Terminal Ubuntu or macOS), navigate to the *root* detectEr directory, and:


<!-- Navigate to the *root* `detecter` directory, and follow these instructions on the console.  -->

<!-- The instructions given are for  -->


<!-- 1. Navigate to the *root* directory `detecter`. -->

1. Change the directory to `examples/erlang` and list its contents.

    ```console
    [duncan@local]:/detecter$ cd examples/erlang
    [duncan@local]:/detecter/examples/erlang$ ls -l
    -rw-r--r--@ 1 duncan  staff  996 May 17 16:57 Makefile
    drwxr-xr-x@ 6 duncan  staff  192 May 14 12:49 props
    drwxr-xr-x@ 3 duncan  staff   96 Apr 29 19:16 src
    ```



2. Type `make` to compile all of the source code files inside the `examples/erlang/src` directory. 
   A new `ebin` directory is created, such that the directory tree now looks as follows.

  XXX

3. 






Analyzing the hellow world program.


Our simple system consists of a 



Follow these steps to test the first

The instructions also 


For now, you can follow these instructions; we elaborate further on what each step entails and the different options detectEr provides when it comes to monitoring.






Consider the very simple system consisting of a process that forks a second process which prints `Hello, `




involves, and what steps one needs to execute to start the monitoring process.


Before using detecter


You can familiarise yourself with the detecter



There are a number of steps involved 


Navigate to the 


detecter/examples directory 











Using the Erlang shell

Using the Elixir shell

Using the Python shell





DetectER The outer make file.

Compiling the simple system of Erlang

Compiling a simple monitor

Instrumenting it and we see some verdict.

## Hello world


