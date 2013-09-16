\name{redisPersist}
\alias{redisPersist}
\title{Clear expiration flags for a key}
\description{Clear expiration flags for a key.}
\usage{
redisPersist(key)
}
\arguments{
  \item{key}{
The (required) character identifier for the value to be looked up.
}
}
\details{
Remove the existing timeout on key, turning the key from volatile (a key with
an expire set) to persistent (a key that will never expire as no timeout is
associated).
}
\value{
1 if the timeout was removed, 0 if the key does not exists or does not have
a timeout set.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisTTL}}, \code{\link{redisExpire}}
}
\examples{
\dontrun{
redisSet('x',runif(5))
redisExpire('x',10)
redisPersist('x')
}
}
