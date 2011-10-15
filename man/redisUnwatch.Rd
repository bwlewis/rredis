\name{redisUnwatch}
\alias{redisUnwatch}
\title{redisUnwatch}
\description{Stop watching the specified keys for changes.}
\usage{
redisUnwatch(keys)
}
\arguments{
  \item{keys}{A character vector or list of key names to stop watching.}
}
\details{
Stop watching the specified keys. See \code{redisWatch} for details.
}
\value{"OK"}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
