\name{redisDelete}
\alias{redisDelete}
\title{Delete a key and associated value from Redis.}
\description{
Delete a key and associated value from Redis.
}
\usage{
redisDelete(key)
}
\arguments{
  \item{key}{
The (required) character identifier to be looked up.
}
}
\details{
The key must not contain spaces or newline characters (otherwise an error will be thrown).
}
\value{
Nothing is returned if the key/value pair is successfully deleted.
A warning is thrown if the they key could not be found.
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
redisDelete('x')
}
}
