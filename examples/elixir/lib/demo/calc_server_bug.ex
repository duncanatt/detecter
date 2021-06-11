defmodule Demo.CalcServerBug do
  @moduledoc """
  Example buggy calculator service.

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
  ### Public API.
  ### --------------------------------------------------------------------------

  @doc """
  Starts server.

  ## Parameters
  - n: The request number to start the server with.

  Returns: Server PID.
  """
  @spec start(n :: integer()) :: pid()
  def start(n) do
    spawn __MODULE__, :loop, [n]
  end


  ### --------------------------------------------------------------------------
  ### Internal callbacks.
  ### --------------------------------------------------------------------------

  @doc """
  Main server loop.

  ## Parameters
  - tot: Total number of serviced requests.

  Returns: Does not return.
  """
  @spec loop(tot :: integer()) :: no_return()
  def loop(tot) do
    receive do
      {clt, {:add, a, b}} ->

        # Handle addition request from client.
        send clt, {:ok, a - b} # Bug!!
        loop tot + 1

      {clt, {:mul, a, b}} ->

        # Handle multiplication request from client.
        send clt, {:ok, a * b}
        loop tot + 1

      {clt, :stp} ->

        # Handle stop request. Server does not loop again.
        send clt, {:bye, tot}
    end
  end

end
