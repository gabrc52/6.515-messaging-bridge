import zephyr
import json
import socket
import os

# The only reason this Python shim exists is because the other way of receiving Zephyr messages
# would involve using Zephyr through the C bindings / FFI, and I spent too much time trying to get it
# to work to no avail, and is probably pretty cumbersome.

# (And it is pointless work if this project might be ported to a different language, probably Racket
# or one of the other Schemes, after the class ends)

# (And it would require reimplementing logic such as zephyr.receive())

# From https://github.com/sipb/matrix-zephyr-bridge/
# Copying various Python files into one

# TODO: make this better / more portable
# TODO: document how to install zephyr???

SOCKET_PORT = 20000

DEFAULT_OPCODE = ''
MATRIX_OPCODE = 'matrix'
DEFAULT_CLASS = 'MESSAGE'
DEFAULT_INSTANCE = 'PERSONAL'
DEFAULT_REALM = 'ATHENA.MIT.EDU'
ZEPHYR_SUBSCRIPTIONS_FILE = 'zephyr.subs'
OWN_KERB = 'daemon/matrix.mit.edu'
DEFAULT_DISPLAY_NAME = 'Matrix'
KEYTAB_PATH = '/home/rgabriel/Projects/uplink/matrix-zephyr-bridge/daemon_matrix.keytab'

def renew_kerberos_tickets():
    """
    Get new working kerberos tickets
    """
    os.system(f"kinit {OWN_KERB}@{DEFAULT_REALM} -k -t {KEYTAB_PATH}")

# TODO: this was copied and pasted, way too complex as we don't even need the parser

# TODO: document how to pip install the library and the fact i had to do the workarounds here
# https://stackoverflow.com/questions/43982543/importerror-no-module-named-cython


class Zephyr:
    """
    Simple Zephyr client that remembers its subscriptions
    with a format similar to .zephyr.subs
    """

    # Which classes are we subscribed to all their instances?
    _entire_class_subscriptions: set[str]
    _subscriptions: zephyr.Subscriptions

    def _subscribe(self, triplet):
        """
        Subscribe (without saving into disk)
        """
        cls,instance,recipient = triplet
        if instance == '*':
            self._entire_class_subscriptions.add(cls)
        if recipient != '*':
            raise ValueError('Non-wildcard recipients are not supported')
        self._subscriptions.add(triplet)
        

    def __init__(self):
        self._entire_class_subscriptions = set()
        renew_kerberos_tickets()
        zephyr.init()
        self._subscriptions = zephyr.Subscriptions()
        with open(ZEPHYR_SUBSCRIPTIONS_FILE, 'r') as f:
            subscriptions = [tuple(line.split(',')) for line in f.read().split('\n') if line != '']
        for triplet in subscriptions:
            self._subscribe(triplet)

    def all_instances_subscribed(self, cls):
        """
        Are we subscribed to all instances of `cls`?
        """
        return cls in self._entire_class_subscriptions
    
    def subscribe(self, cls, instance=None):
        """
        Subscribe to the given Zephyr class and instance
        (equivalent to `zctl add`, in that it persists)

        If instance is not given, assume all instances
        """ 
        triplet = (cls, instance if instance else '*', '*')
        with open(ZEPHYR_SUBSCRIPTIONS_FILE, 'a') as f:
            f.write(','.join(triplet) + '\n')
        self._subscribe(triplet)

# at least initializes stuff, even if we never use this object ever again
z = Zephyr()

def msg_to_json(msg):
    # This is a rather tricky object to serialize
    # The Python library is just bad.
    d = {
        x: getattr(msg, x)
        for x in msg.__dir__()
        if not x.startswith('__')
            and x not in ('uid', # not json serializable, strange object (TODO: if needed, figure out a way to serialize it)
                          'message', 'encoded_message') # redundant? gets parsed into 'fields'
            and not callable(getattr(msg, x))
    }
    return json.dumps(d)

# TODO: does not handle everything THAT well, but it is functional
#   Fix: it can't even know if the connection is dropped
# We can also use a websocket library but it is too high level?
# Alternatively, just call it as a subprocess in scheme (bypassing TCP) and make sure the port works

server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
host = '127.0.0.1'
server.bind((host, SOCKET_PORT))
server.listen(1)
print(f"Listening on {host}:{SOCKET_PORT}... Please connect")
conn, addr = server.accept()
print("Accepted connection!")

try:
    while True:
        # Catch zephyr exceptions at the top level
        try:
            msg: zephyr.ZNotice = zephyr.receive(True)
            if msg is not None:
                json_string = msg_to_json(msg)
                encoded = f"{json_string}\n".encode('utf-8')
                conn.sendall(encoded)
                json_string.encode('utf-8')
        except OSError as e:
            renew_kerberos_tickets()
            print(f"ZEPHYR ERROR: {e}")
except KeyboardInterrupt:
    print("Shutting down...")
finally:
    conn.close()
    server.close()
