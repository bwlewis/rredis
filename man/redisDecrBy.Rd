\name{redisDecrBy}
\alias{redisDecrBy}
\title{Decrement a value by the specified value.}
\description{Decrement the value corresponding to the specified key.
}
\usage{
redisDecrBy(key, value)
}
\arguments{
  \item{key}{A key corresponding to the value to decrement.}
  \item{value}{The value to decrement by (integer, numeric, or character).}
}
\details{
The value should be a character representation of an integer.
If the key does not exist or contains a value of a wrong type, set the 
key to the value of "0" before to perform the operation.
}
\value{
The new value of key after the decrement, returned as a character
string.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisIncr}}
}
\examples{
\dontrun{
redisSet('x','5')
redisDecrBy('x','3')
}
}
