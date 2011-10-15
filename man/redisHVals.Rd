\name{redisHVals}
\alias{redisHVals}
\title{Redis hash values.}
\description{Return all the values associated with the given hash key.}
\usage{
redisHVals(key)
}
\arguments{
  \item{key}{The key to look up.}
}
\details{Returns the values in the Redis hash 
associated with \code{key}.
If the key is not found, or if the list is empty, NULL is returned. If the
key is associated with a value of type other than 'hash,' an error is
thrown.
}
\value{A list of values defined for the given key.  }
\references{ http://redis.io/commands}
\author{ B. W. Lewis }
\seealso{ \code{\link{redisHSet}} }
\examples{
\dontrun{
redisHMSet('A',list(x=1,y=2,z=3))
redisHVals('A')
}
}
