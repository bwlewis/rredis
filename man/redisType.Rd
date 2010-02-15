\name{redisType}
\alias{redisType}
\title{Query a Redis value type.}
\description{Return a description of the type of the value 
corresponding to the specified key.
}
\usage{
redisType(key)
}
\arguments{
  \item{key}{The key to look up.}
}
\details{
The Redis database differentiates three types of values. The value
types determine how Redis operates on the key/value pairs, not what
kind of data may be contained within. Each value type
may store generic R objects, binary blobs, or character strings.
The Redis value types are:

string: A single value is associated with the key

list: A stack of values is associated with the key

set: A set of values is associated with the key.

Values of "list" and "set" types have available to them special stack and
set operations, respectively.
}
\value{
A descriptive character string  that may be one of
"none", "string", "list", or "set". 
"none" is returned if the key does not exist.
}
\references{
http://code.google.com/p/redis/wiki/TypeCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisKeys}}
}
\examples{
\dontrun{
redisConnect()
redisSet('x',5)
redisLPush('y',6)
redisType('x')
redisType('y')
}
}
