\name{redisHMGet}
\alias{redisHMGet}
\title{Retrieve a list of hash values.}
\description{Retrieve a list of hash values from specified key/field pairs at once.}
\usage{
redisHMGet(key, fields)
}
\arguments{
  \item{key}{
The (required) character identifier for the key name.
}
  \item{fields}{
An R string vector of hash fields to retrieve.
}
}
\details{
Redis hash values store values in one or more fields associated with a single
key name. The redisHMGet function retrieves several fields associated with one
key at once. Fields not present return NULL.
}
\value{ A named list of retrieved values.  }
\references{ http://redis.io/commands }
\author{ B. W. Lewis }

\seealso{
\code{\link{redisHSet},\link{redisHMSet}}
}
\examples{
\dontrun{
redisHMSet('A', list(x=1,y=2,z=3))
redisHMGet('A',c('y','z'))
}
}
