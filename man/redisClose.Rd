\name{redisClose}
\alias{redisClose}
\title{Close an open connection to a Redis server.}
\description{The redisClose function closes any open connection to a Redis
server.}
\usage{
redisClose(e)
}
\arguments{
  \item{e}{(Optional) Redis context. The current context is used if e is not supplied.}
}
\details{A running instance of a Redis server is required.  }
\value{Nothing is returned. Errors are displayed if the function fails to
close the connection to the Redis server, or if the connection is invalid.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{redisConnect}}
\code{\link{redisGetContext}}
}
\examples{
\dontrun{
redisConnect()
redisSet('x',runif(5))
redisGet('x')
redisClose()
}
}
