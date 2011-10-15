\name{redisSave}
\alias{redisSave}
\title{redisSave}
\description{Synchronously save that database to disk.
}
\usage{
redisSave()
}
\details{Force Redis to save the database(s) to disk.
}
\value{Nothing is returned. Check the UNIX time of the last completed
save operation with the redisLastsave function.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
