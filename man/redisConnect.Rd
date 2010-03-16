\name{redisConnect}
\alias{redisConnect}
\title{Connect to a Redis server.}
\description{Connect to an available Redis server on the specified port.}
\usage{
redisConnect(host = "localhost", port = 6379, returnRef = FALSE)
}
\arguments{
  \item{host}{The Redis server host name or inet address (optional, character)}
  \item{port}{The Redis port number (optional, numeric or integer)}
  \item{returnRef=FALSE}{Set returnRef=TRUE to return a list describing the Redis connection.}
}
\details{A running instance of a Redis server is required. See
http://code.google.com/p/redis/ for details.}
\value{Nothing is returned by default. 
Errors are displayed if the function fails to
connect to the specified Redis server. Disconnect from a connected server
with redisClose.

If returnRef is set to TRUE and no error occurs, a list describing the 
Redis connection will be returned. The list can be used to manage multiple
Redis connections with the \code{\link{attachRedis}} function.
}
\references{
See http://code.google.com/p/redis/ for details.
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
