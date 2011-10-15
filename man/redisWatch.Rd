\name{redisWatch}
\alias{redisWatch}
\title{redisWatch}
\description{Condition a transaction block on value changes.}
\usage{
redisWatch(keys)
}
\arguments{
  \item{keys}{A character vector or list of key names to watch for changes.}
}
\details{
Watch the specified keys for changed values. If any of the values change
for the watched keys, discard the current transaction block and unwatch
all watched keys.
}
\value{"OK"}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
