--8<-- "includes/common.md"

# Preparation
---


## Prerequisites

This tutorial assumes that users are running Ubuntu/Debian, macOS or Windows 10.
Windows 10 users may either install the [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and follow the instructions given below for *Ubuntu/Debian*, or alternatively, configure [Chocolatey](https://chocolatey.org) and install the software packages mentioned via `choco install`.
However, our instructions will not detail how detectEr is installed and used on Windows systems.

Please install the software package management system available for your particular operating system before proceeding with the rest of the guide.

| Operating System | Package Manager | Installation |
| :--------------: | :-------------: | :----------- |
| Ubuntu/Debian | APT        | Not required, comes pre-bundled              |
| macOS         | Homebrew   | Follow the Homebrew [guide](https://brew.sh) for installation   |
| Windows 10    | Chocolatey | Follow the Chocolatey [guide](https://chocolatey.org/install) for installation |


## Installing Erlang

Erlang can be installed on multiple platforms by compiling it from [source](https://github.com/erlang/otp) or  downloading one of the pre-compiled distributions hosted on [erlang-solutions.com](https://www.erlang-solutions.com/downloads).
We however follow the simpler route and install Erlang using the package managers mentioned above.

[comment]: <> (In this tutorial, we take a simpler route, and install Erlang using the package manager that comes bundled with the operating system.)

=== "Ubuntu/Debian"

    On Ubuntu and Debian, Erlang can be installed using APT.

    1. Add the Erlang Solutions repository:

        ```
        wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
        sudo dpkg -i erlang-solutions_2.0_all.deb
        ```

    2. Refresh the package manager repository cache: `sudo apt update`.

    3. Install the entire Erlang/OTP suite only: `sudo apt install erlang`.

=== "macOS"

    macOS users can install the entire Erlang/OTP suite using Homebrew by running: `brew install erlang`.

=== "Windows 10"

    Windows 10 users not using WSL can install the entire Erlang/OTP suite using Chocolatey by running: `choco install erlang`.

=== "Others"

    Refer to the Erlang Solutions [guide](https://www.erlang-solutions.com/downloads) for installing Erlang/OTP on other operating systems.

!!! hint "Quick check"

    Confirm that Erlang/OTP distribution has been successfully installed and that the system `PATH` variable is properly configured by typing `erl --version`.
    

## Installing Elixir

Elixir runs on the Erlang virtual machine and uses the same OTP libraries.
We already installed the Erlang ecosystem, and need only install the Elixir language libraries.

=== "Ubuntu/Debian"

    On Ubuntu and Debian, Elixir can be installed via APT using: `sudo apt-get install elixir`.

=== "macOS"

    macOS users can install Elixir using Homebrew by running: `brew install elixir`.

=== "Windows 10"

    Windows 10 users not using WSL can install Elixir using Chocolatey by running: `choco install elixir`.

=== "Others"
 
    Refer to the Elixir Lang [guide](https://elixir-lang.org/install.html) for installing Elixir on other operating systems. 

!!! hint "Quick check"

    Confirm that Elixir has been successfully installed and that the system `PATH` variable is properly configured by typing `elixir --version`.

## Installing Python

We also make use of Python 3, that can be installed as explained below.

=== "Ubuntu/Debian"

    Most distributions of Ubuntu and Debian come with Python pre-installed.
    Check the version of Python available on your operating system by executing `python --version`.

    If the reported version is older than `3.7.x` or Python is not installed, follow these steps.

    1. Refresh the package manager repository cache (if not already done): `sudo apt update`.

    2. Install Python 3: `sudo apt install python3.9`.

    3. Install the Python package manager PIP: `sudo apt install python3-pip`.

    4. Also install the core development support tools: `sudo apt install -y build-essential libssl-dev libffi-dev python3-dev`.

=== "macOS"

    macOS comes pre-installed with Python 2.7, which is not the version we use in this tutorial.

    Install Python 3 via Homebrew by typing: `brew install python3`.
    The Python package manager PIP for Python 3 is bundled in the recent versions of Python.

=== "Windows 10"

    Windows 10 users not using WSL can install Python 3 via Chocolatey by typing: `choco install python`.
    The Python package manager PIP for Python 3 is bundled in the recent versions of Python.

=== "Others"

    Refer to the Real Python [guide](https://realpython.com/installing-python) for installing Python 3 on other operating systems. 

!!! hint "Quick check"

    Confirm that Python 3 has been successfully installed and that the system `PATH` variable is properly configured by typing `python3 --version`.
    Also check that PIP for Python 3 has been installed, `pip3 --version`.
    

### Installing the virtual environment management tool

[Virtualenv](https://virtualenv.pypa.io/en/latest) is a tool used to create isolated Python environments.
The environment it creates has its own installation directory, and does not share libraries with other Virtualenv environments.
Virtualenv can be installed via PIP: `pip3 install virtualenv`.


## Configuring Visual Studio Code

Any plain text editor can be used, but for this tutorial we opt for Visual Studio Code that can be downloaded from [https://code.visualstudio.com](https://code.visualstudio.com).
It can also be installed using the respective operating system package manager.

=== "Ubuntu/Debian"

    On Ubuntu and Debian, VS Code can be installed using APT.

    0. Install APT transport for downloading packages via HTTPS: `sudo apt install apt-transport-https` **to check**
    [comment]: <> (   sudo apt install software-properties-common apt-transport-https wget)

    1. Import the Microsoft GPG key: 

        ```
        wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | sudo apt-key add -
        ```

    2. Add the VS Code repository by typing: 

        ```
        sudo add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/code stable main"
        ```

    3. Refresh the package manager repository cache: `sudo apt update`.

    4. Install VS Code: `sudo apt install code`.

=== "macOS"

    macOS users can install VS Code using Homebrew by running: `brew install --cask visual-studio-code`.

=== "Windows 10"

    Windows 10 users not using WSL can install VS Code using Chocolatey by running: `choco install vscode`.

=== "Others"

    Refer to the Visual Studio Code [guide](https://code.visualstudio.com/download) for installing it on other operating systems. 


### Adding extensions

To enable syntax highlighting and language support for the languages used in this tutorial, VS Code can be configured with these extensions. 

* [Erlang](https://marketplace.visualstudio.com/items?itemName=pgourlain.erlang)

* [Elixir](https://marketplace.visualstudio.com/items?itemName=JakeBecker.elixir-ls)

* [Python](https://marketplace.visualstudio.com/items?itemName=ms-python.python)

??? info "Versions"

    These distribution versions are used in this tutorial.
    Your installed versions need not exactly match, but we document these in case of any problems you might face when working through the tutorial later on.

    | Name       | Version   |
    | ---------- | --------- |
    | Erlang/OTP |  `23.3.2` |
    | Elixir     | `1.11.4`  |
    | Python     | `3.9.4`   |
    | PIP        | `21.0.1`  |
    | VS Code    | `1.56.0`  |

---
Now that your system is set up, we can proceed to download and compile detectEr.