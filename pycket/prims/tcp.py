#from rpython.rlib import jit

from pycket.base          import W_Object
from pycket.values        import W_Bool, W_Bytes, W_Cons, W_Fixnum, w_false
from pycket.values_string import W_String
from pycket.values_tcp    import W_TCPSocketWrapper, W_TCPSocket, W_TCPListener
from pycket.error         import SchemeException
from pycket.prims.expose  import expose, default


@expose("tcp-listener?", [W_Object])
def tcp_listener_pred(w_object):
    return W_Bool.make(isinstance(w_object, W_TCPListener))


@expose("tcp-listen", [W_Fixnum, default(W_Fixnum, W_Fixnum(5))])
def tcp_listen(w_port, w_backlog):
    port = w_port.value
    backlog = w_backlog.value
    if port < 0 or port >= 65536:
        raise SchemeException("tcp-listen: port %d out of bounds" % port)
    if backlog <= 0:
        raise SchemeException("tcp-listen: backlog %d <= 0" % backlog)
    w_socket = W_TCPListener.listen(port, backlog)
    if w_socket is None:
        return w_false
    return w_socket


@expose("tcp-close", [W_TCPSocketWrapper])
def tcp_close(w_socket):
    w_socket.close()


@expose("tcp-socket?", [W_Object])
def tcp_socket_pred(w_object):
    return W_Bool.make(isinstance(w_object, W_TCPSocket))


@expose("tcp-accept", [W_TCPListener])
def tcp_accept(w_socket):
    return w_socket.accept()


@expose("tcp-connect", [W_String, W_Fixnum])
def tcp_connect(w_host, w_port):
    host = w_host.tostring()
    port = w_port.value
    if port < 0 or port >= 65536:
        raise SchemeException("TCP port %d out of bounds" % port)
    w_socket = W_TCPSocket.connect(host, port)
    if w_socket is None:
        return w_false
    return w_socket


@expose("tcp-setsockopt-NODELAY-0!", [W_TCPSocket])
def tcp_setsockopt_NODELAY_0(w_socket):
    w_socket.setsockopt_NODELAY(0)


@expose("tcp-setsockopt-NODELAY-1!", [W_TCPSocket])
def tcp_setsockopt_NODELAY_1(w_socket):
    w_socket.setsockopt_NODELAY(1)


@expose("tcp-send", [W_TCPSocket, W_Bytes])
def tcp_send(w_socket, w_bytes):
    data = w_bytes.as_str()
    res = w_socket.send(data)
    return W_Fixnum(res)


@expose("tcp-sendall", [W_TCPSocket, W_Bytes])
def tcp_sendall(w_socket, w_bytes):
    data = w_bytes.as_str()
    w_socket.sendall(data)


@expose("tcp-recv", [W_TCPSocket, W_Fixnum])
def tcp_recv(w_socket, w_size):
    size = w_size.value
    if size <= 0:
        raise SchemeException("tcp-recv: size %d <= 0" % size)
    data = w_socket.recv(size)
    return W_Bytes.from_string(data)


@expose("tcp-recv-chunk", [W_TCPSocket, W_Fixnum, default(W_Fixnum, W_Fixnum(8))])
def tcp_recv_chunk(w_socket, w_size, w_align):
    size  = w_size.value
    align = w_align.value
    if size <= 0:
        raise SchemeException("tcp-recv-chunk: size %d <= 0" % size)
    if align <= 0:
        raise SchemeException("tcp-recv-chunk: align %d <= 0" % align)
    if size % align != 0:
        raise SchemeException("tcp-recv-chunk: size %d no multiple of align %d" % (size, align))
    n, data = w_socket.recv_chunk(size, align)
    return W_Cons.make(W_Fixnum(n), W_Bytes.from_string(data))
