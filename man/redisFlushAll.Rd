\name{redisFlushAll}
\alias{redisFlushAll}
\title{Delete all keys and values from all databases.}
\description{
Delete all keys and values from all databases, not just the currently selected 
one.}
\usage{
redisFlushAll()
}
\value{Returns TRUE if successful, FALSE otherwise.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisFlushDB}}
}
\examples{
\dontrun{
redisConnect()
redisFlushAll()
}
}
