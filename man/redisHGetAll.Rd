\name{redisHGetAll}
\alias{redisHGetAll}
\title{Redis hash fields and values.}
\description{Return all fields and values associated with the given key.}
\usage{
redisHGetAll(key)
}
\arguments{
  \item{key}{The key to look up.}
}
\details{Returns all the fields and their values in the Redis hash 
associated with \code{key}.
If the key is not found, or if the hash is empty, NULL is returned. If the
key is associated with a value of type other than 'hash,' an error is
thrown.
}
\value{A list of values defined for the given key, named by the field names.}
\references{ http://redis.io/commands}
\author{ B. W. Lewis }
\seealso{ \code{\link{redisHSet}} }
\examples{
\dontrun{
redisHMSet('A',list(x=1,y=2,z=3))
redisHGetAll('A')
}
}
