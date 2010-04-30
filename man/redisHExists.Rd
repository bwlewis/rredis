\name{redisHExists}
\alias{redisHExists}
\title{Test the existence of a hash.}
\description{
Test the existence of a hash combination in the Redis database.
}
\usage{
redisHExists(key, field)
}
\arguments{
  \item{key}{ A key name.  }
  \item{field}{ A field name.  }
}
\value{
Returns FALSE if no matching key/field combination, 
TRUE if matching entry exists, and NULL if an error occured.
}
\references{
http://code.google.com/p/redis/HexistsCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisHSet}}
}
\examples{
\dontrun{
redisHSet('A','x',runif(5))
redisHExists('A','x')
}
}
