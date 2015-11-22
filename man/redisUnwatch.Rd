\name{redisUnwatch}
\alias{redisUnwatch}
\title{redisUnwatch}
\description{Stop watching for keys to change.}
\usage{
redisUnwatch()
}
\details{
Stop watching all watched keys. See \code{\link{redisWatch}} for details.
}
\value{"OK"}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
