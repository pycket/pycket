from rpython.rlib.rsocket import INETAddress, RSocket, SocketError, \
                                 AF_INET, SOCK_STREAM, IPPROTO_TCP, TCP_NODELAY

from pycket.base  import W_Object
#from pycket.error import SchemeException


class W_TCPSocketWrapper(W_Object):
    """Generic wrapper around a TCP socket"""
    errorname = "tcp-socket"
    _attrs_ = ["sock"]

    def __init__(self, sock):
        assert isinstance(sock, RSocket)
        self.sock = sock

    def tostring(self):
        return "#<tcp-socket>"

    def close(self):
        self.sock.close()


class W_TCPListener(W_TCPSocketWrapper):
    """Wrapper around a TCP server socket (supports listen/accept)"""

    def __init__(self, sock):
        W_TCPSocketWrapper.__init__(self, sock)

    @staticmethod
    def listen(port, backlog):
        assert isinstance(port, int) and 0 <= port < 65536
        assert isinstance(backlog, int) and backlog > 0
        addr = INETAddress("", port)
        sock = RSocket(AF_INET, SOCK_STREAM)
        try:
            sock.bind(addr)
        except SocketError:
            # Can't bind to port; port may be busy or permissions insufficient
            return None
        sock.listen(backlog)
        return W_TCPListener(sock)

    def accept(self):
        fd, _addr = self.sock.accept()
        sock = RSocket(self.sock.family, self.sock.type, self.sock.proto, fd)
        return W_TCPSocket(sock)


class W_TCPSocket(W_TCPSocketWrapper):
    """Wrapper around a TCP client socket (supports connect/send/recv)"""

    def __init__(self, sock):
        W_TCPSocketWrapper.__init__(self, sock)

    @staticmethod
    def connect(host, port):
        assert isinstance(host, str)
        assert isinstance(port, int) and 0 <= port < 65536
        addr = INETAddress(host, port)
        sock = RSocket(AF_INET, SOCK_STREAM)
        try:
            sock.connect(addr)
        except SocketError:
            # Can't connect to addr; addr may be unreachable or too busy
            return None
        return W_TCPSocket(sock)

    def send(self, data):
        assert isinstance(data, str)
        return self.sock.send(data)
        # XXX: Catch SocketError, raise as SchemeException?

    def sendall(self, data):
        assert isinstance(data, str)
        self.sock.sendall(data)
        # XXX: Catch SocketError, raise as SchemeException?

    def recv(self, size):
        assert isinstance(size, int) and size > 0
        return self.sock.recv(size)
        # XXX: Catch SocketError, raise as SchemeException?

    def recv_chunk(self, size, align):
        assert isinstance(size, int) and isinstance(align, int) and \
               0 < align <= size and size % align == 0
        data = self.sock.recv(size)
        # XXX: Catch SocketError, raise as SchemeException?
        n = len(data)
        if n % align == 0:
            # fast path: data already aligned (or n == 0 due to error or EOF)
            return n, data
        # slow path: read until aligned
        chunks = [data]
        while n % align != 0:
            data = self.sock.recv(align - (n % align))
            # XXX: Catch SocketError, raise as SchemeException?
            k = len(data)
            if k == 0:
                # error or EOF: return chunksize 0
                return 0, ""
            chunks.append(data)
            n += k
        data = "".join(chunks)
        return n, data

    def setsockopt_NODELAY(self, flag):
        assert isinstance(flag, int) and 0 <= flag <= 1
        self.sock.setsockopt_int(IPPROTO_TCP, TCP_NODELAY, flag)
        # XXX: Catch SocketError, raise as SchemeException?
