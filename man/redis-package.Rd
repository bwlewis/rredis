\name{redis-package}
\alias{redis-package}
\alias{redis}
\docType{package}
\title{An R client for Redis}
\description{
Redis is a high-performance, networked, key/value database.
The redis package for R provides a native R language interface
to Redis. The R client can store and read arbitrary serializable R objects 
as well as access raw objects like strings and binary blobs from other clients.
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
B. W. Lewis

Maintainer: B. W. Lewis <blewis@illposed.net>
}
\references{
\link{http://code.google.com/p/redis/}
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
