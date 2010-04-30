\name{redisIncrBy}
\alias{redisIncrBy}
\title{Increment a value by the specified value.}
\description{Increment the value corresponding to the specified key.
}
\usage{
redisIncrBy(key, value)
}
\arguments{
  \item{key}{A key corresponding to the value to increment.}
  \item{value}{The value to increment by (integer, numeric, or character).}
}
\details{
The value should be a character representation of an integer.
If the key does not exist or contains a value of a wrong type, set the 
key to the value of "0" before to perform the operation.
}
\value{
The new value of key after the increment, returned as a character
string.
}
\references{
http://code.google.com/p/redis/wiki/IncrCommand
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisIncr}}
}
\examples{
\dontrun{
redisSet('x',5)
redisIncrBy('x',3)
}
}
