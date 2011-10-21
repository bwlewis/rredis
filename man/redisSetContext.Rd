\name{redisSetContext}
\alias{redisSetContext}
\alias{redisGetContext}
\title{redisSetContext}
\description{Get or set the current active Redis connection environment.}
\usage{
redisSetContext( e = NULL)
redisGetContext()
}
\arguments{
  \item{e}{
    An environment representing the new Redis server connection context
returned by \code{rediscConnect(returnRef=TRUE)}. The default value of NULL
sets the context to the most recently established connection.
  }
}
\details{
The rredis package stores information associated with a connection to
a Redis server in an environment. The \code{redisSetContext} and
\code{redisSetContext} functions help manage simultaneous
connection to multiple Redis servers.

The \code{redisSetContext} function returns an environment representing
the current active Redis connection. If there is no current active Redis
connection, an environment is still returned, but without connection
information.

The \code{redisSetContext} function replaces the current active environment.
Any number of simultaneous connections to multiple Redis servers may be managed
in this manner.
}
\value{
NULL is invisibly returned.
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisGetContext}}
\code{\link{redisConnect}}
}
\examples{
\dontrun{
# Open a connection to a Redis server on HOST1 and store its context:
HOST1 <- redisConnect(host='HOST1', returnRef=TRUE)
print(redisInfo())

# Initiate a new Redis context:
HOST2 <- redisConnect(host='HOST2', returnRef=TRUE)
# The connection to HOST2 is now active:
print(redisInfo())

# We may now switch back and forth between the two active connections:
redisSetContext(HOST1)
print(redisInfo())
redisSetContext(HOST2)
print(redisInfo())
redisClose()
redisSetContext(HOST1)
redisClose()
}
}
