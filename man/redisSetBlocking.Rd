\name{redisSetBlocking}
\alias{redisSetBlocking,redisSetPipeline}
\title{Set the Redis message blocking state.}
\description{
Use \code{redisSetBlocking} to set the rredis client to blocking (default)
or non-blocking Redis communication mode.
}
\usage{
redisSetBlocking( value = TRUE )
redisSetPipeline( value = TRUE )
}
\arguments{
  \item{value}{
  TRUE indicates Redis blocking mode, FALSE non-blocking mode.
}
}
\details{
The rredis client blocks for a response from a connected Redis server
after each transaction in blocking mode. Blocking mode is the default
mode. When in non-blocking mode, transactions are issued without
servicing Redis server responses, and server responses must be manually
serviced with the \code{redisGetResponse} function.

Non-blocking mode can improve performance in some circumstances like
lots of repeated \code{redisSet} operations. When using non-blocking
mode, don't forget to periodically service responses from the Redis
server.

Note that use of commands like \code{redisMSet} can usually obviate the
need to use non-blocking mode.
}
\note{
The function name \code{redisSetBlocking} is deprecated and will be
replaced in a future version by \code{redisSetPipeline}.
}
\value{
The new blocking mode (TRUE/FALSE) is invisibly returned.
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
redisSetBlocking(FALSE)
redisLPush('x',pi)
redisLPush('x',exp(1))
redisGetResponse()
redisSetBlocking(TRUE)
}
}
