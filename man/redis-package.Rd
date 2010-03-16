\name{redis-package}
\alias{redis-package}
\alias{redis}
\docType{package}
\title{An R client for Redis}
\description{
Redis is a high-performance, in-memory, networked, key/value-like database.
The redis package for R provides a native R language interface
to Redis. The R client can store and retrieve arbitrary serializable R objects 
as well as raw data objects like strings and binary blobs from other clients.
Redis supports a number of innovative value types beyond arbitrary
binary data, including sets and lifo/fifo-like lists,
database replication, database persistence, high-performance virtual
memory management and much more. Please see the rredis vignette for 
more information and examples on using Redis and R together.
}
\details{
\tabular{ll}{
Package: \tab redis\cr
Type: \tab Package\cr
Version: \tab 0.01\cr
Date: \tab 2010-01-26\cr
License: \tab Apache\cr
LazyLoad: \tab yes\cr
}
}
\author{
B. W. Lewis, with substantial assistance from Pat Sheilds

Maintainer: B. W. Lewis <blewis@illposed.net>
}
\references{
http://code.google.com/p/redis/
}
\keyword{ package }
\examples{
\dontrun{
redisConnect()
redisSet('x',runif(5))
redisGet('x')
redisClose()
}
}
