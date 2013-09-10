\name{redisGetSet}
\alias{redisGetSet}
\title{Store a value in Redis, returning the previously defined value.}
\description{Store a value identified by a character key name 
in the Redis database, returning the previously defined value or NULL if
the key was not already associated with a value. 
}
\usage{
redisGetSet(key, value, ...)
}
\arguments{
  \item{key}{
The (required) character identifier for the value to be stored. 
}
  \item{value}{ The (required) object to associate with the key. }
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\details{
The key must not contain spaces or newline characters (otherwise an error will be thrown).

The value object is copied to the Redis server.
The value to be stored may can be any
serializable R object up to the Redis maximum object size 
(excluding, for example, external pointer references).
References to other R objects or environments inside the value are not
guaranteed to be preserved.

In order to store strings that can easily be read by other clients, first
convert the character object using the 
\code{\link{charToRaw}} function as shown in the
examples.
}
\value{
The previous value associated with key or NULL if no previous 
association exists.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisGet}}
}
\examples{
\dontrun{
# Store an R object with the key 'x':
redisGetSet('x',runif(5))
}
}
