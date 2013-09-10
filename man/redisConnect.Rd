\name{redisConnect}
\alias{redisConnect}
\title{Connect to a Redis server.}
\description{Connect to an available Redis server on the specified port.}
\usage{
redisConnect(host = "localhost", port = 6379, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
}
\arguments{
  \item{host}{The Redis server host name or inet address (optional, character). The default value is "localhost".}
  \item{port}{The Redis port number (optional, numeric or integer). The default value is 6379L.}
  \item{returnRef}{Set returnRef=TRUE to return the environment that contains the Redis connection state (see details). The default value is FALSE.}
  \item{nodelay}{Set nodelay=TRUE to disbable the TCP Nagle algorithm (see details). The default value is FALSE.}
  \item{timeout}{Set the R connection timeout (in seconds).
          Beware that some OSes may treat very large
          values as zero: however the POSIX standard requires values up
          to 31 days to be supported.}
}
\details{
A running instance of a Redis server is required.
Use `returnRef` to return the Redis connection state in an environment.
Then use the `redisSetContext` function to switch environment state and
manage multiple open Redis connections.

Set \code{nodelay=FALSE} to disable the TCP Nagle flow control algorithm which
can improve performance especially for rapid, non-pipelined small-sized
transactions. However, we discourage users from setting this.
Possible consequences include:

* A flooded network with many small Redis messages

* Unhappy TCP services other than Redis

* Angry system adiministrators.

A better choice for good performance is to enable Redis pipelining
using \code{redisSetPipeline(FALSE)} and then use the \code{redisGetResponse}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

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
