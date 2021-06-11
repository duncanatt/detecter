defmodule Demo.CalcClient do
  @moduledoc """
  Example client exposing API to the calculator service.

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
  Issues addition request to server.

  ## Parameters
  - a: First number to add.
  - b: Second number to add.

  Returns: Result of addition.
  """
  @spec add(a :: number(), b :: number()) :: number()
  def add(a, b) do
    {:add, res} = rpc Demo.CalcServer, {:add, a, b}
    res
  end

  @doc """
  Issues multiplication request to server.

  ## Parameters
  - a: First number to multiply.
  - b: Second number to multiply.

  Returns: Result of multiplication.
  """
  @spec mul(a :: number(), b :: number()) :: number()
  def mul(a, b) do
    {:mul, res} = rpc Demo.CalcServer, {:mul, a, b}
    res
  end

  @doc """
  Issues a synchronous request that blocks until a response is received.

  ## Parameters
  - to: PID or registered name of target process.
  - req: Request to issue.

  Returns: Response.
  """
  @spec rpc(to :: pid() | atom(), req :: any()) :: any()
  def rpc(to, req) do
    send to, {self(), req}
    receive do
      resp ->
        resp
    end
  end

end


