\name{redisSetContext}
\alias{redisSetContext}
\title{redisSetContext}
\description{Set the current active Redis connection environment.}
\usage{
redisSetContext( e = new.env())
}
\arguments{
  \item{e}{
    An environment representing the new context, defaults to a new context environment.
  }
}
\details{
The rredis package stores everything associated with a connection to
a Redis server in an environment. The \code{redisSetContext} and
\code{redisSetContext} functions help manage simultaneous
connection to multiple Redis servers.

The \code{redisSetContext} function returns an environment representing
the current active Redis connection. If there is no current active Redis
connection, an environment is still returned, but without connection
information.

The \code{redisSetContext} function can initialize a new Redis connection
environment or replace the current active environment with one previously
referenced by \code{redisSetContext}. Any number of simultaneous connections
to multiple Redis servers may be managed in this manner.
}
\value{
NULL is invisibly returned.
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisGetContext}}
}
\examples{
\dontrun{
# Open a connection to a Redis server on HOST_1 and store its context:
redisConnect(host='HOST_1')
HOST_1 <- redisGetContext()

# Initiate a new Redis context:
redisSetContext()
redisConnect(host='HOST_2')
HOST_2 <- redisGetContext()

# We may now switch back and forth between the two active connections:
redisSetContext(HOST_1)
redisKeys()
redisSetContext(HOST_2)
redisKeys()
redisClose()
redisSetContext(HOST_1)
redisClose()
}
}
