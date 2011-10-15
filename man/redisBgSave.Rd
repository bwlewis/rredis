\name{redisBgSave}
\alias{redisBgSave}
\title{redisBgSave}
\description{Asynchronously save that database to disk.
}
\usage{
redisBgSave()
}
\details{Force Redis to save the database(s) to disk in the background.
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
