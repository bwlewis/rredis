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
  \item{...}{ Optional additional arguments passed to \code{redisCmd}.
     Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\details{
The key must not contain spaces or newline characters (otherwise an error will
be thrown). The \code{raw} option is used to retrieve binary data from other
languages.
}
\value{
The value corresponding to the specified key, or NULL if the matching key
contained no value or if no matching key was found.

Use the \code{raw=TRUE} option to return Redis values in raw binary form, which
can optionally later be converted to character using the \code{rawToChar}
function.

A Redis value that does not represent a serialized R value is returned as a
\code{character} R value with an attribute named "redis string value." Subsequent
uploads of that value to Redis will send the character-valued data in raw form
(not as a serialized R value), preserving the original nature of the data in
Redis.
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
