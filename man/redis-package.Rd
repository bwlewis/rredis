\name{redis-package}
\alias{redis-package}
\alias{redis}
\docType{package}
\title{An R client for Redis}
\description{
Redis is a high-performance networked 
persistent key/value structured data store.
The redis package for R provides an easy to use native R language interface
to Redis. The R client can store and read arbitrary serializable R objects 
as well as access raw objects like strings from other clients. The redis
package presently supports only a useful subset of the many available Redis
features. A more full-featured client implementation is in process.
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
