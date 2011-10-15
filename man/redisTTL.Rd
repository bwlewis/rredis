\name{redisTTL}
\alias{redisTTL}
\title{Return the time to live for a key set to expire.}
\description{She the time left to live in seconds for the specified key.}
\usage{
redisTTL(key)
}
\arguments{
  \item{key}{The key to look up.}
}
\details{Redis keys may be set to exipre at or after a given time with
the \code{redisExpire} and \code{redisExpireAt} functions. This function
shows the time left before exipraton in seconds for such a key.
}
\value{Integer time left to live or -1 if the key is not set to expire or
not found.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisExpire}}, \code{\link{redisExpireAt}}
}
\examples{
\dontrun{
redisConnect()
redisSet('x',5)
redisExpire('x',100)
redisTTL('x')
}
}
