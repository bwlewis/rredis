# Minimalist socket functions
.SOCK_POLLIN  = 1L
.SOCK_POLLPRI = 2L
.SOCK_POLLOUT = 4L

# Return vector of sockets matching requested events. Negative
# socket numbers in returned vector indicates sockets with error
# conditions.
.SOCK_POLL = function(fds, timeout=1000L, events=.SOCK_POLLIN)
{
  x = .Call('SOCK_POLL', as.integer(fds), as.integer(timeout), as.integer(events), PACKAGE='rredis')
  c(fds[x == events], -fds[x>4])
}

.SOCK_CLOSE = function(socket)
{
  .Call('SOCK_CLOSE', as.integer(socket), PACKAGE='rredis')
}

.SOCK_RECV = function(socket, external_pointer=FALSE, buf_size=8192, max_buffer_size=2^24)
{
  .Call('SOCK_RECV', as.integer(socket), as.integer(external_pointer), as.integer(buf_size), as.numeric(max_buffer_size), PACKAGE='rredis')
}

.SOCK_GETLINE = function(socket)
{
  .Call('SOCK_GETLINE', as.integer(socket), PACKAGE='rredis')
}

.SOCK_RECV_N = function(socket, N)
{
  .Call('SOCK_RECV_HTTP_HEAD', as.integer(socket), as.integer(N), PACKAGE='rredis')
}

# We trap the possibility of a SIGPIPE signal error during SOCK_SEND.
.SOCK_SEND = function(socket, msg)
{
  if(is.raw(msg)) return(
    tryCatch(.Call('SOCK_SEND', socket, msg, PACKAGE='rredis'),
      error=function(e) -1, interrupt=function(e) -1))
  if(is.character(msg))
    return(
      tryCatch(
        .Call('SOCK_SEND', socket, charToRaw(msg), PACKAGE='rredis'),
        error=function(e) -1, interrupt=function(e) -1))
  stop("msg must be of data type 'Raw'")
}

.SOCK_GETSOCKNAME = function(socket)
{
  .Call('SOCK_NAME', as.integer(socket), PACKAGE='rredis')
}

.SOCK_CONNECT = function(host, port)
{
  .Call('SOCK_CONNECT', as.character(host), as.integer(port),
        PACKAGE='rredis')
}
