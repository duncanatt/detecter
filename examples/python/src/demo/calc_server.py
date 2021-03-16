""" Example calculator service over TCP.

Issuing commands to the server can be done using a utility such as telnet. The
following commands are supported:
- `/add A B\r\n`: Addition request, where A and B are integers or floating point
                  numbers. Returns the result of addition to the client,
                  followed by `\r\n`.
- `/mul A B\r\n`: Multiplication request, where A and B are integers or floating
                  point numbers. Returns the result of multiplication to the
                  client, followed by `\r\n`.
- `/stop`: Stops the server. Returns a confirmation to the client, followed by
           `\r\n`.

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
import socket
import re
import logging
import threading
import argparse

HOST = '0.0.0.0'  # Server host.
PORT = 8080  # Server port.
ENC = 'ascii'  # Encoding used to parse bytes to and from the socket byte stream.
P_ID = 1024  # Parent thread party (or process) ID.

# Configure global logging.
logging.basicConfig(
    filename='test.log', format='%(message)s', filemode='w', level=logging.DEBUG
)

# Get main logger.
log = logging.getLogger(__name__)


def start(mode='ok'):
    """ Starts server.

    :param mode: Server operation mode.
    :return: Server thread ID.
    """
    err_fact = 0 if mode == 'ok' else 1
    server = threading.Thread(target=loop, args=[err_fact])
    server.start()

    log.debug('fork({p_id},{id},{{{mod},{fun},{args}}})'.format(
        p_id=to_pid(P_ID), id=to_pid(PORT),
        mod='calc_server', fun='loop', args=[err_fact]
    ))

    log.debug('exit({p_id},normal)'.format(p_id=to_pid(P_ID)))
    return server.ident


def loop(err_fact):
    """ Main server loop.

    :param err_fact: Error factor used to displace and return the correct or
                     incorrect result of the calculation.
    :return: Does not return.
    """
    calc_proto = re.compile('^/(?P<cmd>add|mul) (?P<a>[0-9]*(.[0-9]+)?) (?P<b>[0-9]*(.[0-9]+)?)\r\n$')
    stop_proto = re.compile('^/(?P<cmd>stop)\r\n$')

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind((HOST, PORT))
        port = sock.getsockname()[1]
        print('Started server on {0}:{1}..'.format(HOST, port))

        log.debug('init({id},{p_id},{{{mod},{fun},{args}}})'.format(
            id=to_pid(port), p_id=to_pid(P_ID),
            mod='calc_server', fun='loop', args=[err_fact]
        ))

        # Listen for client connections.
        sock.listen()
        conn, (remote_ip, remote_port) = sock.accept()
        print('Connected by {0}:{1}.'.format(remote_ip, remote_port))

        while True:
            data = conn.recv(1024)

            if not data:
                break

            # Parse data from bytes using encoding and match to regex, and
            # handle request.
            enc_data = str(data, ENC)

            if req := calc_proto.match(enc_data):
                cmd, a, b = req['cmd'], float(req['a']), float(req['b'])
                print('Received command /{0} {1} {2}.'.format(cmd, a, b))

                log.debug('recv({id},{{{remote_id},{{{cmd},{a},{b}}}}})'.format(
                    id=to_pid(port), remote_id=to_pid(remote_port),
                    cmd=cmd, a=a, b=b
                ))

                # Perform calculation.
                if cmd == 'add':
                    res = a + b + err_fact
                elif cmd == 'mul':
                    res = a * b + err_fact

                # Send reply to client.
                conn.send((str(res) + '\r\n').encode(ENC))

                log.debug('send({id},{remote_id},{{{cmd},{res}}})'.format(
                    id=to_pid(port), remote_id=to_pid(remote_port),
                    cmd=cmd, res=res
                ))
            elif req := stop_proto.match(enc_data):
                cmd = req['cmd']
                print('Received command /{0}..shutting down.'.format(cmd))

                log.debug('recv({id},{{{remote_id},{cmd}}})'.format(
                    id=to_pid(port), remote_id=to_pid(remote_port), cmd=cmd
                ))

                # Send reply to client and stop.
                conn.send('stopped\r\n'.encode(ENC))

                log.debug('send({id},{remote_id},{{ok,stopped}})'.format(
                    id=to_pid(port), remote_id=to_pid(remote_port)
                ))
                log.debug('exit({id},normal)'.format(id=to_pid(port)))
                break
            else:
                print('WARN: unknown request {0}'.format(enc_data))
                log.debug('recv({id},{{{remote_id},"{req}"}})'.format(
                    id=to_pid(port), remote_id=to_pid(remote_port),
                    req=enc_data.strip()
                ))


def to_pid(id):
    """ Converts the specified ID to an Erlang-like PID.

    :param id: ID to convert.
    :return: Converted ID.
    """
    return '<0.{0}.0>'.format(id)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Starts the example calculator service over TCP'
    )
    parser.add_argument('Mode', metavar='mode', choices=['ok', 'buggy'],
                        type=str, help='Normal and buggy operation')

    # Parse server mode argument and start main server thread.
    args = parser.parse_args()
    start(args.Mode)
