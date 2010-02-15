\name{redisSet}
\alias{redisSet}
\title{Store a value in Redis.}
\description{Store a value identified by a character key name 
in the Redis database. Any existing value with the same key will be replaced
by the new value.
}
\usage{
redisSet(key, value, NX = FALSE)
}
\arguments{
  \item{key}{
The (required) character identifier for the value to be stored. 
}
  \item{value}{
The (required) object to associate with the key. 
}
  \item{NX}{
If NX = TRUE, existing values will not be replaced.
}
}
\details{
The key must not contain spaces or newline characters (otherwise an error will be thrown).

The value is copied to the Redis server.
The value to be stored can be any
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
The character string "OK" is returned on success. 
If NX = FALSE and a value already exists, the value will not be replaced
and FALSE will be returned.
}
\references{
http://code.google.com/p/redis/wiki/SetCommand
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
redisSet('x',runif(5))
}
\dontrun{
# Store a string that can be easily read by other clients:
redisSet('x',charToRaw('Hello Redis clients'))
}
}
