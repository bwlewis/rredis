\name{redisGet}
\alias{redisGet}
\title{Retrieve a value from Redis.}
\description{Retrieve a value identified by a key 
from the Redis database.
}
\usage{
redisGet(key)
}
\arguments{
  \item{key}{
The (required) character identifier for the value to be looked up.
}
}
\details{
The key must not contain spaces or newline characters (otherwise an error will be thrown).
}
\value{
The retrieved value is returned, or NULL if no matching key was found.
}
\references{
{\link{http://code.google.com/p/redis/}}
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisSet}}
}
\examples{
\dontrun{
redisStore('x',runif(5))
redisGet('x')
}
}
