\name{redisHLen}
\alias{redisHLen}
\title{Redis hash length.}
\description{Return the number of fields associated with the given key.}
\usage{
redisHLen(key)
}
\arguments{
  \item{key}{The key to look up.}
}
\details{Returns the number of fields in the Redis hash 
associated with \code{key}.
If the key is not found, or if the hash is empty, 0 is returned. If the
key is associated with a value of type other than 'hash,' an error is
thrown.
}
\value{The number of fields defined for the given key.  }
\references{ http://code.google.com/p/redis/wiki/HlenCommand }
\author{ B. W. Lewis }
\seealso{ \code{\link{redisHSet}} }
\examples{
\dontrun{
redisHMSet('A',list(x=1,y=2,z=3))
redisHLen('A')
}
}
