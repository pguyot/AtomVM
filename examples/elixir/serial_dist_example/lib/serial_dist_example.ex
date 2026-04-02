defmodule SerialDistExample do
  @moduledoc """
  Example: distributed Erlang over serial (UART) using Circuits.UART on BEAM.

  This example demonstrates how to connect a BEAM node to an AtomVM node
  over a serial link using the `serial_dist` distribution module and
  `Circuits.UART` for serial port access.

  ## Quick start with socat (virtual serial ports)

      # Terminal 1: create virtual serial port pair
      socat -d -d pty,raw,echo=0 pty,raw,echo=0
      # Note the two /dev/pts/N paths printed

      # Terminal 2: start the BEAM node
      cd examples/elixir/serial_dist_example
      mix deps.get
      SERIAL_DEVICE=/dev/pts/3 mix run --no-halt -e "SerialDistExample.start()"

      # Terminal 3: start the AtomVM node
      SERIAL_DEVICE=/dev/pts/4 ./build/src/AtomVM ./build/examples/erlang/serial_disterl.avm

  ## With real serial ports

      SERIAL_DEVICE=/dev/ttyUSB0 mix run --no-halt -e "SerialDistExample.start()"

  ## Multi-port example

  Connect to two AtomVM nodes simultaneously:

      SERIAL_DEVICES=/dev/ttyUSB0,/dev/ttyUSB1 mix run --no-halt -e "SerialDistExample.start()"

  """

  @doc """
  Start serial distribution on this BEAM node.

  Reads the serial device path from the `SERIAL_DEVICE` (single port) or
  `SERIAL_DEVICES` (comma-separated list) environment variable.
  """
  def start do
    uart_configs = uart_configs()

    {:ok, _} =
      :net_kernel.start(:"beam@serial.local", %{
        name_domain: :longnames,
        proto_dist: :serial_dist,
        avm_dist_opts: %{
          uart_ports: uart_configs,
          uart_module: :circuits_uart_hal
        }
      })

    :erlang.set_cookie(:SerialTest)

    IO.puts("Distribution started over serial")
    IO.puts("Node: #{node()}")
    IO.puts("Cookie: #{:erlang.get_cookie()}")
    Process.register(self(), :serial_dist_example)

    IO.puts("Registered as 'serial_dist_example'. Waiting for messages.")

    IO.puts(
      "From the peer:\n  {serial_dist_example, :'#{node()}'} ! {hello, node()}."
    )

    loop()
  end

  @doc """
  Start as a test peer. Used by CI to test AtomVM<->BEAM interop.

  Starts serial distribution, sends a ping to the AtomVM peer, and
  prints the result to stdout for the test harness to verify.
  """
  def beam_peer do
    pty_path =
      System.get_env("PTY_PATH") ||
        raise "PTY_PATH environment variable not set"

    {:ok, _} =
      :net_kernel.start(:"beam_peer@serial.local", %{
        name_domain: :longnames,
        proto_dist: :serial_dist,
        avm_dist_opts: %{
          uart_opts: [peripheral: pty_path, speed: 115_200],
          uart_module: :circuits_uart_hal
        }
      })

    :erlang.set_cookie(:SerialTest)
    peer_node = :"atomvm_peer@serial.local"

    # Send ping to the AtomVM peer
    {test_serial, peer_node} |> send({self(), :ping})

    receive do
      {_pid, :pong} ->
        IO.puts("pong")

      other ->
        IO.puts("unexpected: #{inspect(other)}")
    after
      30_000 ->
        IO.puts("timeout")
        System.halt(1)
    end
  end

  defp uart_configs do
    cond do
      devices = System.get_env("SERIAL_DEVICES") ->
        devices
        |> String.split(",", trim: true)
        |> Enum.map(fn dev ->
          [{:peripheral, String.trim(dev)}, {:speed, 115_200}]
        end)

      device = System.get_env("SERIAL_DEVICE") ->
        [[{:peripheral, device}, {:speed, 115_200}]]

      true ->
        IO.puts("Error: set SERIAL_DEVICE or SERIAL_DEVICES env var")
        IO.puts("  e.g. SERIAL_DEVICE=/dev/ttyUSB0")
        IO.puts("  e.g. SERIAL_DEVICES=/dev/ttyUSB0,/dev/ttyUSB1")
        System.halt(1)
    end
  end

  defp loop do
    receive do
      :quit ->
        IO.puts("Received quit, stopping.")

      {:hello, from} ->
        IO.puts("Hello from #{inspect(from)}!")
        loop()

      {:ping, from} when is_pid(from) ->
        send(from, {:pong, node()})
        loop()

      other ->
        IO.puts("Received: #{inspect(other)}")
        loop()
    end
  end
end
