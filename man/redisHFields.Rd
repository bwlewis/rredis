\name{redisHFields}
\alias{redisHFields}
\title{Redis hash fields.}
\description{Return the fields associated with the given key.}
\usage{
redisHFields(key)
}
\arguments{
  \item{key}{The key to look up.}
}
\details{Returns the fields in the Redis hash 
associated with \code{key}.
If the key is not found, or if the hash is empty, NULL is returned. If the
key is associated with a value of type other than 'hash,' an error is
thrown.
}
\value{A list of fields defined for the given key.  }
\references{ http://code.google.com/p/redis/wiki/HfieldsCommand }
\author{ B. W. Lewis }
\seealso{ \code{\link{redisHSet}} }
\examples{
\dontrun{
redisHMSet('A',list(x=1,y=2,z=3))
redisHFields('A')
}
}
