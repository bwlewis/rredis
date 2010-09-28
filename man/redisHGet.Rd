\name{redisHGet}
\alias{redisHGet}
\title{Retrieve a hased value from Redis.}
\description{Retrieve a value identified by a key and field
from the Redis database.
}
\usage{
redisHGet(key, field, raw = FALSE)
}
\arguments{
  \item{key}{
A key name.
}
  \item{field}{
A field name.
}
  \item{raw}{
Set \code{raw=TRUE} to skip de-serialization of the data.
}
}
\details{
Redis hash values store values in one or more fields associated with a single
key name.
}
\value{
The value corresponding to the specified key/field,
or NULL if the matching key/field hash contained no value 
or if no matching key or field was found.
}
\references{
http://code.google.com/p/redis/wiki/HgetCommand
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
redisHGet('A','x')
}
}
