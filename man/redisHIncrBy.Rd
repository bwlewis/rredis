\name{redisHIncrBy}
\alias{redisHIncrBy}
\title{Increment a value.}
\description{Increment the value corresponding to the given key/field
combination by the specified value.
}
\usage{
redisHIncrBy(key, field, value)
}
\arguments{
  \item{key}{A key name.}
  \item{field}{A field name.}
  \item{value}{The value to increment by (integer, numeric, or character).}
}
\details{
The value should be a character representation of an integer.
If the key/field value does not exist or contains a 
value of a wrong type, set the 
key to the value of "0" before to perform the operation.
}
\value{
The new value of key after the increment, returned as a character
string.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisHSet}}
}
\examples{
\dontrun{
redisHSet('A','x',5)
redisHIncrBy('A','x',3)
}
}
