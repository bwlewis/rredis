\name{redisExists}
\alias{redisExists}
\title{Test the existence of a key in the Redis database.}
\description{
Test the existence of a key in the Redis database.
}
\usage{
redisExists(key)
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
Returns FALSE if no matching key, TRUE if matching key exists, and NULL
if an error occured.
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
redisExists('x')
}
}
