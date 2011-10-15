\name{redisExec}
\alias{redisExec}
\title{redisExec}
\description{End a transaction block.}
\usage{
redisExec()
}
\details{End a transaction block. All Redis statements issued after
\code{redisMulti} will be queued locally and then sent to the Redis
server en masse once the \code{redisExec} function is issued.
}
\value{The results of all queued functions are returned in a list.}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
