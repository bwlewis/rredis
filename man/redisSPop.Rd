\name{redisSPop}
\alias{redisSPop}
\title{Remove and return an element from a set.}
\description{
Remove and return an element, chosen at random, from the specified set.
}
\usage{
redisSPop(set, ...)
}
\arguments{
  \item{set}{The set name (character).}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\value{A random element of the set, or NULL if the set is empty or does
not exist.}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSAdd}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd('set',runif(2))
redisSAdd('set',runif(3))
redisSAdd('set',runif(4))
redisSPop('set')
redisSPop('set')
redisSPop('set')
redisSPop('set')
}
}
