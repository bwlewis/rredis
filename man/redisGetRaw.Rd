\name{redisGetRaw}
\alias{redisGetRaw}
\title{Retrieve a raw vector value from Redis.}
\description{Retrieve a raw vector value identified by a key 
from the Redis database.
}
\usage{
redisGetRaw(key)
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
The value corresponding to the specified key, 
or NULL if the matching key contained no value or if no matching key was found.
}
\references{
http://code.google.com/p/redis/wiki/GetCommand
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisSet}}
}
\examples{
\dontrun{
redisSet('x',charToRaw('testing'))
y <- redisGetRaw('x')
rawToChar(y)
}
}
