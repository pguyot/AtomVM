<!--
 Copyright 2025 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Distributed Erlang

For a generic introduction to Distributed Erlang Systems, please refer to the [dedicated section](https://www.erlang.org/doc/system/distributed.html) of Erlang/OTP documentation.

AtomVM provides an implementation of Erlang distribution protocol and AtomVM nodes can take part in clusters with both AtomVM and BEAM nodes.

Distribution is currently available on all platforms with TCP/IP communication, namely:
- Generic Unix
- ESP32
- RP2 (Pico)

Distribution over serial (UART) is also available for point-to-point
connections between any two nodes, including microcontrollers without
networking (e.g. STM32). See [Serial distribution](#serial-distribution).

Three examples are provided:

- disterl in `examples/erlang/disterl.erl`: distribution on Unix systems
- epmd\_disterl in `examples/erlang/esp32/epmd_disterl.erl`: distribution on ESP32 devices
- serial\_disterl in `examples/erlang/serial_disterl.erl`: distribution over serial (ESP32 and Unix)

## Starting and stopping distribution

Distribution has to be started programmatically. Following Erlang/OTP, distribution relies on `kernel` which is started by `init:boot/1`.

The following lines will start distribution on Unix systems with long name `atomvm@127.0.0.1`.

```erlang
{ok, _NetKernelPid} = net_kernel:start('atomvm@127.0.0.1', #{name_domain => longnames}),
ok = net_kernel:set_cookie(<<"AtomVM">>).
```

`net_kernel:stop/0` can be used to stop distribution.

### Distribution options

The options map passed to `net_kernel:start/2` supports an `avm_dist_opts` key containing a map of options that are forwarded to the distribution module's `listen/2` function.

#### socket_dist options

The built-in `socket_dist` module supports the following `avm_dist_opts`:

- `listen_port_min` — minimum port number to listen on
- `listen_port_max` — maximum port number to listen on

Both must be specified together. `socket_dist` will try each port in the range until one is available. This is useful on systems where only a specific range of ports is open (e.g. firewall rules on embedded devices).

```erlang
{ok, _NetKernelPid} = net_kernel:start(mynode, #{
    name_domain => shortnames,
    avm_dist_opts => #{listen_port_min => 9100, listen_port_max => 9110}
}).
```

When `avm_dist_opts` is omitted or the port keys are not set, the OS assigns an ephemeral port (the default behaviour).

## `epmd`

AtomVM nodes can use Erlang/OTP's epmd on Unix systems. AtomVM is also bundled with a pure Erlang implementation of `epmd` which can be used on all platforms. Module is called `epmd`, to be distinguished from `erl_epmd` which is the client.

AtomVM's epmd daemon can be started with:

```erlang
{ok, _EPMDPid} = epmd:start_link([]).
```

This has to be called before invoking `net_kernel:start/2`.

## Erlang/OTP compatibility

AtomVM can connect to Erlang/OTP 24 and higher.

## Security

AtomVM supports cookie authentication. However, distribution over TLS is not supported yet.

## Alternative carrier

Following Erlang/OTP, AtomVM supports alternative carriers with distribution modules. Please refer to [Erlang/OTP's dedicated documentation](https://www.erlang.org/doc/apps/erts/alt_dist#distribution-module).

The main difference is that packets exchanged by `f_recv` and `f_send` handlers must be binaries instead of list of integers, for memory usage reasons.

AtomVM's `f_send` has the following signature:

```erlang
fun (DistCtrlr, Data :: binary()) -> ok | {error, Error}
```

AtomVM's `f_recv` has the following signature:

```erlang
fun (DistCtrlr, Length :: pos_integer(), Timeout :: timeout()) -> {ok, Packet} | {error, Reason}
```

AtomVM's distribution is based on `socket_dist` and `socket_dist_controller` modules which can also be used with BEAM by definining `BEAM_INTERFACE` to adjust for the difference.

## Serial distribution

AtomVM supports distribution over serial (UART) connections using the
`serial_dist` module. This is useful for microcontrollers that lack
WiFi/TCP (e.g. STM32) but have UART, and for testing distribution
locally using virtual serial ports.

### Quick start

```erlang
{ok, _} = net_kernel:start('mynode@serial.local', #{
    name_domain => longnames,
    proto_dist => serial_dist,
    avm_dist_opts => #{
        uart_opts => [{peripheral, "UART1"}, {speed, 115200},
                      {tx, 17}, {rx, 16}],
        uart_module => uart
    }
}).
```

On Unix, the `peripheral` is a device path such as `"/dev/ttyUSB0"` and
the `uart_module` is `uart` from the `avm_unix` library.

### serial\_dist options

- `uart_opts` — proplist passed to `UartModule:open/1` (see `uart_hal`
  for common parameters: `peripheral`, `speed`, `data_bits`, `stop_bits`,
  `parity`, `flow_control`)
- `uart_module` — module implementing the `uart_hal` behaviour. Defaults
  to `uart`.

### Wire protocol

Serial distribution layers three protocols on the raw UART byte stream:

**1. Sync frames (link layer)**

Both sides periodically send 2-byte sync frames (`<<16#AA, 16#55>>`) on
the UART. These serve two purposes:

- **Liveness detection**: a node knows its peer is alive when it receives
  sync frames.
- **Garbage collection**: after a failed handshake attempt, stale bytes
  from the old handshake remain in the UART buffer. Sync frames are
  stripped from all received data, so stale data that happens to contain
  `16#AA 16#55` pairs is harmlessly consumed, and non-sync stale bytes
  eventually get discarded when the next handshake attempt interprets
  them as an invalid packet.

The sync magic (`16#AA 16#55`) is chosen so it cannot appear as the
first two bytes of a valid length-prefixed handshake message (handshake
messages have a 16-bit big-endian length prefix, and the longest
handshake message is well under 100 bytes, so the first byte is always
`0x00`).

**2. Handshake packets (2-byte length prefix)**

During the Erlang distribution handshake, messages are framed as:

```
<<Length:16/big, Payload:Length/binary>>
```

This matches the TCP distribution handshake format. The handshake
follows the standard Erlang distribution protocol (send\_name,
send\_status, send\_challenge, send\_challenge\_reply,
send\_challenge\_ack).

**3. Data packets (4-byte length prefix)**

After the handshake completes, distribution data packets use a 4-byte
length prefix:

```
<<Length:32/big, Payload:Length/binary>>
```

Tick (keepalive) messages are sent as `<<0:32>>` (4 zero bytes).

### Peer-to-peer connection model

Unlike TCP distribution which uses a client/server model (one side
listens, the other connects), serial is point-to-point: both nodes
share a single UART link.

A **link manager** process on each node is the sole owner of UART
reads. On each iteration it:

1. Checks its mailbox for a `setup` request from `net_kernel`
   (non-blocking).
2. Sends a sync frame.
3. Reads from the UART with a short timeout.
4. Strips sync frames from received data.
5. If handshake data remains, enters the **accept** path (responder).
6. If a `setup` request was pending, enters the **setup** path
   (initiator).
7. Otherwise, loops.

This design ensures only one process reads from the UART at any time,
avoiding the race condition that would occur if separate accept and
setup processes competed for the same byte stream.

If a handshake fails (the distribution controller process exits), the
link manager flushes stale `setup` messages from its mailbox and
restarts the loop, allowing retries.

### Testing with socat

On Unix, `socat` can create virtual serial port pairs for testing:

```bash
socat -d -d pty,raw,echo=0 pty,raw,echo=0
```

This creates two pseudo-terminal devices (e.g. `/dev/ttys003` and
`/dev/ttys004`) connected back-to-back. Each AtomVM node uses one side:

```erlang
%% Node A
{ok, _} = net_kernel:start('a@serial.local', #{
    name_domain => longnames,
    proto_dist => serial_dist,
    avm_dist_opts => #{
        uart_opts => [{peripheral, "/dev/ttys003"}, {speed, 115200}],
        uart_module => uart
    }
}).

%% Node B (separate AtomVM process)
{ok, _} = net_kernel:start('b@serial.local', #{
    name_domain => longnames,
    proto_dist => serial_dist,
    avm_dist_opts => #{
        uart_opts => [{peripheral, "/dev/ttys004"}, {speed, 115200}],
        uart_module => uart
    }
}).

%% From Node B, trigger autoconnect:
{some_registered_name, 'a@serial.local'} ! {self(), hello}.
```

## Distribution features

Distribution implementation is (very) partial. The most basic features are available:
- serialization of all types
- epmd protocol (client and server)
- message passing
- monitoring processes
- I/O distribution ("group leader").

RPC (remote procedure call) from Erlang/OTP to AtomVM is also supported.

Shell requires several OTP standard library modules. See [the example project](https://github.com/pguyot/atomvm_shell).

Please do not hesitate to file issues or pull requests for additional features.
