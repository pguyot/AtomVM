%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(socket).

-export([
    open/3,
    close/1,
    bind/2,
    listen/1, listen/2,
    accept/1,
    accept/2,
    sockname/1,
    peername/1,
    recv/1,
    recv/2,
    recvfrom/1,
    recvfrom/2,
    send/2,
    sendto/3,
    setopt/3,
    connect/2
]).

%% internal nifs
-export([nif_select_read/3, nif_accept/1, nif_recv/1, nif_recvfrom/1, nif_select_stop/1]).

-type socket_domain() :: inet.
-type socket_type() :: stream.
-type socket_protocol() :: tcp.
-type socket() :: term().
-type sockaddr() :: sockaddr_in().
-type sockaddr_in() :: #{
    family := inet,
    port := port_num(),
    addr := addr()
}.
-type addr() :: any | loopback | address_v4().
-type address_v4() :: {octet(), octet(), octet(), octet()}.
-type octet() :: 0..255.
-type port_num() :: 0..65535.

-export_type([socket/0]).

-define(DEFAULT_BACKLOG, 4).

%% -define(TRACE(Fmt, Args), io:format(Fmt, Args)).
-define(TRACE(Fmt, Args), ok).

%%-----------------------------------------------------------------------------
%% @param   Domain the network domain
%% @param   Type the network type
%% @param   Protocol the network protocol
%% @returns `{ok, Socket}' if successful; `{error, Reason}', otherwise.
%% @doc     Create a socket.
%%
%%          Create a socket with a specified domain, type, and protocol.
%%          Use the returned socket for communications.
%%
%%          Currently, the supported values are as follows:
%%          <ul>
%%              <li>`Domain': `inet'</li>
%%              <li>`Type': `stream'</li>
%%              <li>`Protocol': `tcp'</li>
%%          </ul>
%%
%% Example:
%%
%%          `{ok, ListeningSocket} = socket:open(inet, stream, tcp)'
%% @end
%%-----------------------------------------------------------------------------
-spec open(Domain :: socket_domain(), Type :: socket_type(), Protocol :: socket_protocol()) ->
    {ok, socket()} | {error, Reason :: term()}.
open(_Domain, _Type, _Protocol) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Close a socket.
%%
%%          Close a previously opened socket.
%%
%% Example:
%%
%%          `ok = socket:close(Socket)'
%% @end
%%-----------------------------------------------------------------------------
-spec close(Socket :: socket()) -> ok | {error, Reason :: term()}.
close(_Socket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Address the address to which to bind the socket
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Bind a socket to an interface.
%%
%%          Bind a socket to an interface, via a socket address.  Use `any' to
%%          bind to all interfaces.  Use `loopback' to bind to the loopback
%%          address.  To specify a port, use a map containing the network family,
%%          address, and port.
%%
%% Example:
%%
%%          `ok = socket:bind(ListeningSocket, #{family => inet, addr => any, port => 44404})'
%% @end
%%-----------------------------------------------------------------------------
-spec bind(Socket :: socket(), Address :: sockaddr() | any | loopback) ->
    ok | {error, Reason :: term()}.
bind(_Socket, _Address) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Set the socket to listen for connections.
%%
%%          Listen for connections.  The socket should be a connection-based
%%          socket and should be bound to an address and port.
%%
%% Example:
%%
%%          `ok = socket:listen(ListeningSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec listen(Socket :: socket()) -> ok | {error, Reason :: term()}.
listen(Socket) ->
    ?MODULE:listen(Socket, ?DEFAULT_BACKLOG).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Backlog the maximum length for the queue of pending connections
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Set the socket to listen for connections.
%%
%%          Listen for connections.  The socket should be a connection-based
%%          socket and should be bound to an address and port.
%%
%%          Use the Backlog to specify the maximum length for the queue of
%%          pending connections
%%
%% Example:
%%
%%          `ok = socket:listen(ListeningSocket, 4)'
%% @end
%%-----------------------------------------------------------------------------
-spec listen(Socket :: socket(), Backlog :: non_neg_integer()) -> ok | {error, Reason :: term()}.
listen(_Socket, _Backlog) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Address}' if successful; `{error, Reason}', otherwise.
%% @doc     Return the current address for the specified socket.
%%
%% Example:
%%
%%          `{ok, Address} = socket:sockname(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec sockname(Socket :: socket()) -> {ok, Address :: sockaddr()} | {error, Reason :: term()}.
sockname(_Socket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Address}' if successful; `{error, Reason}', otherwise.
%% @doc     Return the address of the peer connected to the specified socket.
%%
%% Example:
%%
%%          `{ok, Address} = socket:peername(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
peername(_Socket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Connection}' if successful; `{error, Reason}', otherwise.
%% @doc     Wait for the socket to accept a connection.
%%
%%          Equivalent to `accept(Socket, infinity)'
%%
%% Example:
%%
%%          `{ok, ConnectedSocket} = socket:accept(ListeningSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec accept(Socket :: socket()) -> {ok, Connection :: socket()} | {error, Reason :: term()}.
accept(Socket) ->
    accept(Socket, infinity).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Timeout timeout (in milliseconds)
%% @returns `{ok, Connection}' if successful; `{error, Reason}', otherwise.
%% @doc     Wait for the socket to accept a connection.
%%
%%          Wait for the socket to accept a connection.  The socket should
%%          be set to listen for connections.
%%
%%          Note that this function will block until a connection is made
%%          from a client.  Typically, users will spawn a call to `accept'
%%          in a separate process.
%%
%% Example:
%%
%%          `{ok, ConnectedSocket} = socket:accept(ListeningSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec accept(Socket :: socket(), Timeout :: non_neg_integer()) ->
    {ok, Connection :: socket()} | {error, Reason :: term()}.
accept(Socket, Timeout) ->
    Self = self(),
    Ref = erlang:make_ref(),
    ?TRACE("select read for accept.  self=~p ref=~p~n", [Self, Ref]),
    case ?MODULE:nif_select_read(Socket, Self, Ref) of
        ok ->
            receive
                {select, _AcceptedSocket, SelectReadRef, ready_input} when SelectReadRef == Ref ->
                    case ?MODULE:nif_accept(Socket) of
                        {error, closed} = E ->
                            ?MODULE:nif_select_stop(Socket),
                            E;
                        R ->
                            R
                    end;
                {closed, ClosedRef} when ClosedRef == Ref ->
                    {error, closed}
            after Timeout ->
                {error, timeout}
            end;
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Data}' if successful; `{error, Reason}', otherwise.
%% @doc     Receive data on the specified socket.
%%
%%          Equivalent to `recv(Socket, infinity)'
%%
%% Example:
%%
%%          `{ok, Data} = socket:recv(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec recv(Socket :: socket()) -> {ok, Data :: binary()} | {error, Reason :: term()}.
recv(Socket) ->
    recv(Socket, infinity).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Timeout timeout (in milliseconds)
%% @returns `{ok, Data}' if successful; `{error, Reason}', otherwise.
%% @doc     Receive data on the specified socket.
%%
%%          Note that this function will block until data is received
%%          on the socket.
%%
%% Example:
%%
%%          `{ok, Data} = socket:recv(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec recv(Socket :: socket(), Timeout :: non_neg_integer()) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.
recv(Socket, Timeout) ->
    Self = self(),
    Ref = erlang:make_ref(),
    ?TRACE("select read for recv.  self=~p ref=~p~n", [Self, Ref]),
    case ?MODULE:nif_select_read(Socket, Self, Ref) of
        ok ->
            receive
                {select, _AcceptedSocket, SelectReadRef, ready_input} when SelectReadRef == Ref ->
                    case ?MODULE:nif_recv(Socket) of
                        {error, closed} = E ->
                            ?MODULE:nif_select_stop(Socket),
                            E;
                        R ->
                            R
                    end;
                {closed, ClosedRef} when ClosedRef == Ref ->
                    {error, closed}
            after Timeout ->
                {error, timeout}
            end;
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, {Address, Data}}' if successful; `{error, Reason}', otherwise.
%% @doc     Receive data on the specified socket, returning the from address.
%%
%%          Equivalent to `recvfrom(Socket, infinity)'
%%
%% Example:
%%
%%          `{ok, {Address, Data}} = socket:recvfrom(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec recvfrom(Socket :: socket()) ->
    {ok, {Address :: sockaddr(), Data :: binary()}} | {error, Reason :: term()}.
recvfrom(Socket) ->
    recvfrom(Socket, infinity).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Timeout timeout (in milliseconds)
%% @returns `{ok, {Address, Data}}' if successful; `{error, Reason}', otherwise.
%% @doc     Receive data on the specified socket, returning the from address.
%%
%%          Note that this function will block until data is received
%%          on the socket.
%%
%% Example:
%%
%%          `{ok, {Address, Data}} = socket:recvfrom(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec recvfrom(Socket :: socket(), Timeout :: non_neg_integer()) ->
    {ok, {Address :: sockaddr(), Data :: binary()}} | {error, Reason :: term()}.
recvfrom(Socket, Timeout) ->
    Self = self(),
    Ref = erlang:make_ref(),
    % io:format("select read for recvfrom.  self=~p ref=~p~n", [Self, Ref]),
    case ?MODULE:nif_select_read(Socket, Self, Ref) of
        ok ->
            receive
                {select, _AcceptedSocket, SelectReadRef, ready_input} when SelectReadRef == Ref ->
                    case ?MODULE:nif_recvfrom(Socket) of
                        {error, closed} = E ->
                            ?MODULE:nif_select_stop(Socket),
                            E;
                        R ->
                            R
                    end;
                {closed, ClosedRef} when ClosedRef == Ref ->
                    {error, closed}
            after Timeout ->
                {error, timeout}
            end;
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Data the data to send
%% @returns `{ok, Rest}' if successful; `{error, Reason}', otherwise.
%% @doc     Send data on the specified socket.
%%
%%          Note that this function will block until data is sent
%%          on the socket.  The data may not have been received by the
%%          intended recipient, and the data may not even have been sent
%%          over the network.
%%
%% Example:
%%
%%          `{ok, []} = socket:send(ConnectedSocket, Data)'
%% @end
%%-----------------------------------------------------------------------------
-spec send(Socket :: socket(), Data :: iolist()) ->
    {ok, Rest :: binary()} | {error, Reason :: term()}.
send(_Socket, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Data the data to send
%% @param   Dest the destination to which to send the data
%% @returns `{ok, Rest}' if successful; `{error, Reason}', otherwise.
%% @doc     Send data on the specified socket to the specified destination.
%%
%%          Note that this function will block until data is sent
%%          on the socket.  The data may not have been received by the
%%          intended recipient, and the data may not even have been sent
%%          over the network.
%%
%% Example:
%%
%%          `{ok, []} = socket:sendto(ConnectedSocket, Data, Dest)'
%% @end
%%-----------------------------------------------------------------------------
-spec sendto(Socket :: socket(), Data :: iolist(), Dest :: sockaddr()) ->
    {ok, Rest :: binary()} | {error, Reason :: term()}.
sendto(_Socket, _Data, _Dest) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Option the option
%% @param   Value the option value
%% @returns `{ok, Address}' if successful; `{error, Reason}', otherwise.
%% @doc     Set a socket option.
%%
%%          Set an option on a socket.
%%
%%          Currently, the following options are supported:
%%          <table>
%%              <tr><td>`{socket, reuseaddr}'</td><td>`boolean()'</td></tr>
%%              <tr><td>`{socket, linger}'</td><td>`#{onoff => boolean(), linger => non_neg_integer()}'</td></tr>
%%          </table>
%%
%% Example:
%%
%%      `ok = socket:setopt(ListeningSocket, {socket, reuseaddr}, true)'
%%      `ok = socket:setopt(ListeningSocket, {socket, linger}, #{onoff => true, linger => 0})'
%% @end
%%-----------------------------------------------------------------------------
setopt(_Socket, _Option, _Value) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Connection}' if successful; `{error, Reason}', otherwise.
%% @doc     Wait for the socket to connect to an address.
%%
%%          Wait for the socket to connect to an address.  The socket should
%%          be a connection-based socket.
%%
%%          Note that this function will block until a connection is made
%%          to a server.
%%
%% Example:
%%
%%          `ok = socket:connect(Socket, #{family => inet, addr => loopback, port => 44404})'
%% @end
%%-----------------------------------------------------------------------------
-spec connect(Socket :: socket(), Address :: addr()) -> ok | {error, Reason :: term()}.
connect(_Socket, _Address) ->
    erlang:nif_error(undefined).
%%
%% Internal functions
%%

%% @private
nif_select_read(_Socket, _Self, _Ref) ->
    erlang:nif_error(undefined).

%% @private
nif_accept(_Socket) ->
    erlang:nif_error(undefined).

%% @private
nif_recv(_Socket) ->
    erlang:nif_error(undefined).

%% @private
nif_recvfrom(_Socket) ->
    erlang:nif_error(undefined).

%% @private
nif_select_stop(_Socket) ->
    erlang:nif_error(undefined).
