\name{rredis-package}
\alias{rredis}
\docType{package}
\title{ Redis interface package.  }
\description{

The \code{rredis} package provides a native R interface to Redis.  Redis is an
in memory key/value database with many innovative features, see
\url{http://redis.io/} for details.  It supports data persistence, networked
client/server operation, command pipelining, structured value types, data
expiration, multicast-like publish/subscribe, and it's very fast. 

}
\details{
Package options:
\itemize{
  \item \code{options('redis:num'=TRUE)} Set this option to TRUE to return
Redis \code{:} messages as numeric values. This was the default behavior
of the rredis package for all versions prior to 1.6.9. For versions
of the R package later than that, redis \code{:} messages are returned
as raw Redis string values to correspond to the data types stored in Redis.
Set this option to revert to the old behavior.

Redis commands affected by this option importantly include the increment
and decrement operations.
}
}
\author{
B. W. Lewis <blewis@illposed.net>

Maintainer: B. W. Lewis <blewis@illposed.net>
}
\keyword{ package }
