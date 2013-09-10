\name{redisGet}
\alias{redisGet}
\title{Retrieve a value from Redis.}
\description{Retrieve a value identified by a key 
from the Redis database.
}
\usage{
redisGet(key, ...)
}
\arguments{
  \item{key}{
The (required) character identifier for the value to be looked up.
}
  \item{...}{ Optional additional arguments.
Specify \code{raw=TRUE} to skip de-serialization of the data.
}
}
\details{
The key must not contain spaces or newline characters (otherwise an error will be thrown). The \code{raw} option is used to retrieve binary data from other
languages.
}
\value{
The value corresponding to the specified key, 
or NULL if the matching key contained no value or if no matching key was found.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisSet}}
}
\examples{
\dontrun{
redisSet('x',runif(5))
redisGet('x')
}
}
