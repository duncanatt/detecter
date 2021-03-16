defmodule Demo.CalcServer do
  @moduledoc """
  Example calculator service.

  Copyright (c) 2021, Duncan Paul Attard <duncanatt@gmail.com>

  This program is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>.
  """

  ### --------------------------------------------------------------------------
  ### Macro and record definitions.
  ### --------------------------------------------------------------------------

  ## Normal and buggy operation constants.
  @mode_ok :ok
  @mode_buggy :buggy


  ### --------------------------------------------------------------------------
  ### Type definitions.
  ### --------------------------------------------------------------------------

  @type mode() :: :ok | :buggy


  ### --------------------------------------------------------------------------
  ### Public API.
  ### --------------------------------------------------------------------------

  @doc """
  Starts server

  ## Parameters
  - mode: Server operation mode.

  The server operates in one of two modes: `ok', where it handles calculations
  correctly, and `buggy', to return incorrect results.

  Returns: Server PID.
  """
  @spec start(mode :: mode()) :: pid()
  def start(mode) do
    Process.register pid =
                       spawn(
                         __MODULE__,
                         :loop,
                         [
                           cond do
                             mode === @mode_ok -> 0;
                             mode === @mode_buggy -> 1 end
                         ]
                       ), __MODULE__
    pid
  end

  @doc """
  Stops server.

  Returns: `stopped' to indicate successful termination.
  """
  @spec stop() :: :ok
  def stop() do
    {:ok, status} = Demo.CalcClient.rpc __MODULE__, :stop
    status
  end

  ### --------------------------------------------------------------------------
  ### Internal callbacks.
  ### --------------------------------------------------------------------------

  @doc """
  Main server loop.

  ## Parameters
  - err_fact: Error factor used to displace and return the correct or incorrect
              result of the calculation.

  Returns: Does not return.
  """
  @spec loop(err_fact :: number()) :: no_return()
  def loop(err_fact) do
    receive do
      x = {from, {:add, a, b}} ->

        # Handle addition request from client.
        send from, {:add, a + b + err_fact}
        loop err_fact

      {from, {:mul, a, b}} ->

        # Handle multiplication request from client.
        send from, {:mul, a * b + err_fact}
        loop err_fact

      {from, :stop} ->

        # Handle stop request. Server does not loop again.
        send from, {:ok, :stopped}

      any ->
        IO.puts "WARN: unknown request #{inspect any}."
        loop err_fact
    end
  end

end
