\name{redisConnect}
\alias{redisConnect}
\title{Connect to a Redis server.}
\description{Connect to an available Redis server on the specified port.}
\usage{
redisConnect(host = "localhost", port = 6379, password = NULL,
             returnRef = FALSE, nodelay=TRUE, timeout=2678399L, closeExisting=TRUE)
}
\arguments{
  \item{host}{The Redis server host name or inet address (optional, character). The default value is "localhost".}
  \item{port}{The Redis port number (optional, numeric or integer). The default value is 6379L.}
  \item{password}{Redis authentication password.}
  \item{returnRef}{Set \code{returnRef=TRUE} to return the environment that contains the Redis connection state (see details). The default value is FALSE.}
  \item{nodelay}{Set \code{nodelay=TRUE} to use TCP_NODELAY
      (that is, to disbable the TCP Nagle algorithm). The default value is
       \code{TRUE}, see the details below.}
  \item{timeout}{Set the R connection timeout (in seconds).
          Beware that some OSes may treat very large
          values as zero: however the POSIX standard requires values up
          to 31 days to be supported.}
  \item{closeExisting}{Set to \code{FALSE} to keep any existing Redis connection open (for use with \code{setContext}).}
}
\details{
A running instance of a Redis server is required.  Use `returnRef` to return
the Redis connection state in an environment.  Then use the `redisSetContext`
function to switch environment state and manage multiple open Redis
connections.

Set \code{nodelay=TRUE} to use the TCP_NODELY socket setting (disabling the TCP
Nagle flow control algorithm) which can improve performance especially for
rapid, non-pipelined small-sized transactions. We follow the convention of
other popular Redis clients like the hiredis C library interface and use
TCP_NODELAY as the default choice.

Note that Redis pipelining can also increase performance:
\code{redisSetPipeline(TRUE)} (q.v.).

}

\value{Nothing is returned by default. 
Errors are displayed if the function fails to
connect to the specified Redis server. Disconnect from a connected server
with redisClose.

If returnRef is set to TRUE and no error occurs, a list describing the 
Redis connection will be returned. A future version of the package will
use this feature to support multiple Redis connections with 
the \code{attachRedis} function.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisClose}}
\code{\link{redisGetContext}}
\code{\link{redisSetContext}}
\code{\link{redisSetPipeline}}
\code{\link{redisGetResponse}}
}
\examples{
\dontrun{
redisConnect()
redisSet('x',runif(5))
redisGet('x')
redisClose()
}
}
