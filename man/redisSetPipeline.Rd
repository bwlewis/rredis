\name{redisSetPipeline}
\alias{redisSetPipeline}
\title{Set the Redis message blocking state.}
\description{
Use \code{redisSetPipeline} to set the rredis client to blocking (default)
or non-blocking Redis communication mode.
}
\usage{
redisSetPipeline( value = FALSE )
}
\arguments{
  \item{value}{TRUE indicates Redis pipelined mode, FALSE non-pipelined mode.}
}
\details{
The rredis client blocks for a response from a connected Redis server
after each transaction in non-pipelined mode (the default).
When in pipelined mode, transactions are issued without
servicing Redis server responses, and server responses must be manually
serviced with the \code{redisGetResponse} function.

Pipelined mode can improve performance in some circumstances like
lots of repeated \code{redisSet} operations. When using pipelined
mode, don't forget to periodically service responses from the Redis
server (results are cached on the server until requested).

Note that use of commands like \code{redisMSet} can sometimes obviate the
need to use non-blocking mode.
}
\value{
The new pipeline mode (TRUE/FALSE) is invisibly returned.
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisGetResponse}}
}
\examples{
\dontrun{
redisConnect()
redisSetPipeline(TRUE)
redisLPush('x',pi)
redisLPush('x',exp(1))
redisGetResponse()
redisSetPipeline(FALSE)
}
}
