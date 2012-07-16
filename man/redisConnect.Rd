\name{redisConnect}
\alias{redisConnect}
\title{Connect to a Redis server.}
\description{Connect to an available Redis server on the specified port.}
\usage{
redisConnect(host = "localhost", port = 6379, returnRef = FALSE, timeout = 2147483647L)
}
\arguments{
  \item{host}{The Redis server host name or inet address (optional, character)}
  \item{port}{The Redis port number (optional, numeric or integer)}
  \item{returnRef}{Set returnRef=TRUE to return a list describing the Redis connection (not presently useful).}
  \item{timeout}{Optional TCP connection timeout value in seconds (integer).}
}
\details{A running instance of a Redis server is required.}

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
}
\examples{
\dontrun{
redisConnect()
redisSet('x',runif(5))
redisGet('x')
redisClose()
}
}
