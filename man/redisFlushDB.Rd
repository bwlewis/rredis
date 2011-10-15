\name{redisFlushDB}
\alias{redisFlushDB}
\title{Delete all keys and values from the current database.}
\description{
Delete all keys and values from the currently selected database.}
\usage{
redisFlushDB()
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
\code{\link{redisFlushAll}}
}
\examples{
\dontrun{
redisConnect()
redisFlushDB()
}
}
