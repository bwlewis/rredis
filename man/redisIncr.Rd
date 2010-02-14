\name{redisIncr}
\alias{redisIncr}
\title{Increment a value by one.}
\description{Increment the value corresponding to the specified key by one.
}
\usage{
redisIncr(key)
}
\arguments{
  \item{key}{A key corresponding to the value to increment.
}
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
\code{\link{redisDecr}}
}
\examples{
\dontrun{
redisStore('x',runif(5))
redisIncr('x')
}
}

