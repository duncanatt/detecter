--8<-- "includes/common.md"

# Setting up detectEr
---


## Downloading detectEr

detectEr can be cloned from our GitHub repository:

```
git clone https://github.com/duncanatt/detecter.git
```

You can also download detectEr in archive format by visiting [https://github.com/duncanatt/detecter](https://github.com/duncanatt/detecter), or by running:

```
wget -O detecter.zip https://github.com/duncanatt/detecter/archive/refs/heads/master.zip
unzip detecter.zip
mv detecter-master detecter
```

!!! hint "Unzip"
    If the `unzip` utility is not installed on your Ubuntu/Debian system, you can install it via APT by running `sudo apt install unzip`. On macOS, `unzip` is pre-installed.

Both of these actions should result in the creation of the directory `detecter` that is structured as follows.

| Directory name                                                                                    | Description                                                    |
| :------------------------------------------------------------------------------------------------ | :------------------------------------------------------------- |
| `detecter`                                                                                        | {++detectEr root directory++}                                  |
| &nbsp;&nbsp;&nbsp;&nbsp;:material-chevron-right:{.material-icon}`detecter`                        | *detectEr tool directory*                                      |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`priv`                                            | detectEr language specification and other configuration files  |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`src`                                             | Erlang module sources                                          |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`test`                                            | Unit tests                                                     |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`include`                                         | Supporting macros                                              |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Makefile`                                        | Makefile with targets for compiling and testing detectEr       |
| &nbsp;&nbsp;&nbsp;&nbsp;:material-chevron-right:{.material-icon}`examples`                        | *Tutorial examples*                                            |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;:material-chevron-right:{.material-icon}`erlang`  | *Calculator server implementation in Erlang*                   |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`src`                     | Erlang module sources                                          |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`props`                   | sHML property specifications                                   |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Makefile`                | Makefile with targets for compiling the example                |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;:material-chevron-right:{.material-icon}`elixir`  | *Calculator server implementation in Elixir*                   |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`lib`                     | Elixir module sources                                          |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`props`                   | sHML property specifications                                   |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Makefile`                | Makefile with targets for compiling the example                |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;:material-chevron-right:{.material-icon}`python`  | *Calculator server implementation in Python using TCP sockets* |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`src`                     | Python script sources                                          |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`props`                   | sHML property specifications                                   |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Makefile`                | Makefile with targets for compiling the example                |
| `LICENSE`                                                                                         | GPL3 license                                                   |

## Compiling detectEr

For this tutorial, we do not use sophisticated code building mechanisms such as rebar, but stick to the `make` utility to standardise our build processes.

=== "Ubuntu/Debian"

    On Ubuntu/Debian, GNU `make` can be installed using APT.

    1. Refresh the package manager repository cache: `sudo apt update`.

    2. Install make: `sudo apt install make`.

    Alternatively, you may choose to install the full `build-essential` package which includes `make`. 

=== "macOS"

    macOS users need to download and install the Apple Developer Command Line Tools which includes `make`.
    
    1. Run the command: `xcode-select --install`.

    2. In the ensuing dialog, click *Install* and agree to the Terms of Service. 

    You can also install GNU `make` using Homebrew by typing: `brew install make`.

=== "Windows 10"

    Windows 10 users not using WSL can install GNU `make` via Chocolatey by running: `choco install make`.

detectEr and its accompanying code examples comes bundled with the necessary makefiles to facilitate their use. To compile detectEr, navigate to the *root* `detecter` project directory.

1. Change the current directory to the `detecter` tool directory: `cd detecter`.

2. Execute `make`.

A new `ebin` directory is created, containing the compiled Erlang source modules as `*.beam` executables. 
The `detecter` directory now looks as follows:

| Directory name                     | Description                                                   |
| :---------                         | :------------------------------------------------------------ |
| :material-chevron-right:{.material-icon}`detecter`                         | detectEr tool directory                                       |
| &nbsp;&nbsp;&nbsp;&nbsp;`priv`     | detectEr language specification and other configuration files |
| &nbsp;&nbsp;&nbsp;&nbsp;`src`      | Erlang module sources                                         |
| &nbsp;&nbsp;&nbsp;&nbsp;`test`     | Unit tests                                                    |
| &nbsp;&nbsp;&nbsp;&nbsp;`include`  | Supporting macros                                             |
| &nbsp;&nbsp;&nbsp;&nbsp;`ebin`     | {==Compiled detectEr files==}                                 |
| &nbsp;&nbsp;&nbsp;&nbsp;`Makefile` | Makefile with targets for compiling and testing detectEr      |

---
With detectEr compiled, let us move on to our first 'hello world' example!