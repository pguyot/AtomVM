#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

"""Local endpoints for the network tests, so they do not leave the machine.

CI runner egress is slow and lossy enough that connections to an external
site fail on their own: on esp32 they pile up in lwIP until the board runs
out of memory, and on generic_unix a TLS handshake under valgrind gives up.

- http port: answers any request with an HTTP/1.1 301
- tls port:  TLS (self-signed, clients don't verify), answers HTTP/1.1 200

Used by the esp32 qemu tests, which reach this host at 10.0.2.2 through
SLIRP user networking and expect the default ports, and by the estdlib
test_ssl suite, which is told where to connect through the environment.

Usage: python3 tests/local_test_servers.py [--certdir DIR] [--http-port N]
                                          [--tls-port N] [--bind ADDR] [--cn NAME]
Generates a self-signed certificate with the openssl CLI if none is found.
"""

import argparse
import os
import socketserver
import ssl
import subprocess
import threading

def make_http_response(cn):
    return (
        b"HTTP/1.1 301 Moved Permanently\r\n"
        b"Content-Length: 0\r\n"
        b"Location: https://" + cn.encode() + b"/\r\n"
        b"Connection: close\r\n"
        b"\r\n"
    )

HTTPS_BODY = b"ok"
HTTPS_RESPONSE = (
    b"HTTP/1.1 200 OK\r\n"
    b"Content-Type: text/plain\r\n"
    b"Content-Length: " + str(len(HTTPS_BODY)).encode() + b"\r\n"
    b"Connection: close\r\n"
    b"\r\n" + HTTPS_BODY
)


class ReusableTCPServer(socketserver.ThreadingTCPServer):
    allow_reuse_address = True
    daemon_threads = True


def make_http_handler(response):
    class HTTPHandler(socketserver.BaseRequestHandler):
        def handle(self):
            try:
                self.request.settimeout(10)
                self.request.recv(2048)
                self.request.sendall(response)
            except OSError:
                pass

    return HTTPHandler


def make_https_handler(context):
    class HTTPSHandler(socketserver.BaseRequestHandler):
        def handle(self):
            try:
                # The qemu guest computes the ECDHE/ECDSA handshake in
                # software emulation; under the memory-check build a step can
                # take tens of seconds.
                self.request.settimeout(120)
                with context.wrap_socket(self.request, server_side=True) as tls:
                    tls.recv(2048)
                    tls.sendall(HTTPS_RESPONSE)
                    # Let the client send its close_notify first: closing
                    # right after the reply can turn into an RST that races
                    # the client's shutdown and fails its close.
                    while tls.recv(2048):
                        pass
            except (OSError, ssl.SSLError) as exc:
                print("tls error:", repr(exc), flush=True)

    return HTTPSHandler


def ensure_certificate(certdir, cn):
    cert = os.path.join(certdir, "cert.pem")
    key = os.path.join(certdir, "key.pem")
    if not (os.path.exists(cert) and os.path.exists(key)):
        # ECDSA P-256, like github.com: the esp32 qemu targets fail RSA
        # handshakes (MBEDTLS_ERR_RSA_PUBLIC_FAILED, the emulated hardware
        # bignum unit), while the ECDSA path is exercised on every green run.
        subprocess.run(
            [
                "openssl", "req", "-x509", "-newkey", "ec",
                "-pkeyopt", "ec_paramgen_curve:P-256", "-nodes",
                "-keyout", key, "-out", cert, "-days", "30",
                "-subj", "/CN=" + cn,
            ],
            check=True,
            capture_output=True,
        )
    return cert, key


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--certdir", default=".")
    parser.add_argument("--http-port", type=int, default=80)
    parser.add_argument("--tls-port", type=int, default=443)
    parser.add_argument("--bind", default="0.0.0.0")
    parser.add_argument("--cn", default="10.0.2.2")
    args = parser.parse_args()

    cert, key = ensure_certificate(args.certdir, args.cn)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    # Same parameters a github.com TLS 1.2 handshake uses: the esp32 qemu
    # targets only enable TLS 1.2 and their emulated crypto hardware is
    # exercised (and green) on exactly this suite/curve combination.
    context.minimum_version = ssl.TLSVersion.TLSv1_2
    context.maximum_version = ssl.TLSVersion.TLSv1_2
    context.set_ciphers("ECDHE-ECDSA-AES128-GCM-SHA256")
    context.set_ecdh_curve("prime256v1")
    context.load_cert_chain(cert, key)

    http_server = ReusableTCPServer(
        (args.bind, args.http_port), make_http_handler(make_http_response(args.cn))
    )
    https_server = ReusableTCPServer((args.bind, args.tls_port), make_https_handler(context))

    threading.Thread(target=http_server.serve_forever, daemon=True).start()
    print(
        "listening on %s:%d (http) and %s:%d (tls)"
        % (args.bind, args.http_port, args.bind, args.tls_port),
        flush=True,
    )
    https_server.serve_forever()


if __name__ == "__main__":
    main()
