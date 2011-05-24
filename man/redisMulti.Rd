\name{redisMulti}
\alias{redisMulti}
\title{redisMulti}
\description{Start a transaction block.}
\usage{
redisMulti()
}
\details{Begin a transaction block. All Redis statements issued after
\code{redisMulti} will be queued locally and then sent to the Redis
server en masse once the \code{redisExec} function is issued.
}
\value{"OK" indicates the transaction has begun.}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
