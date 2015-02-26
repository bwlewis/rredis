\name{Increment, Decrement functions}
\alias{redisIncr}
\alias{redisIncrBy}
\alias{redisIncrByFloat}
\alias{redisDecr}
\alias{redisDecrBy}
\title{Increment or decrement Redis values.}
\description{
\code{redisIncr} increments the Redis string value corresponding to the
specified key by one.

\code{redisDecr} decrements the Redis string value corresponding to the
specified key by one.

The various *By functions increment or decrement values by a specified
integer or numeric value.
}
\usage{
redisIncr(key)
redisIncrBy(key, value)
redisIncrByFloat(key, value)
redisDecr(key)
redisDecrBy(key, value)
}
\arguments{
  \item{key}{A key corresponding to the value to increment.}
  \item{value}{The value to increment by (integer, numeric, or character).}
}
\details{
Note that they initial value must be a raw character value (a plain Redis
string value), not a serialized R value. See the examples below.

The increment value may be an integer (\code{redisIncrBy}) or a numeric value
(\code{redisIncrByFloat}), or a raw character representation of an integer or
numeric value.  If the key does not exist or contains a value of a wrong type,
set the key to the value of "0" or similar string representation of an integer
before running the function.

}
\value{
The new value of key after the increment. Note that the value is returned as
a raw Redis string value.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisDecr}}
}
\examples{
\dontrun{
redisSet('x',charToRaw('5'))  # Note the value must be a raw string!
redisIncr('x')
redisIncrBy('x','3')
redisIncrBy('x',3)  # also works
redisIncrByFloat('x',pi)
redisDecr('x')
redisDecrBy('x',3)
}
}
