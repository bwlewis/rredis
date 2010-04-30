\name{redisMGet}
\alias{redisMGet}
\title{Retrieve one or more values from Redis.}
\description{Retrieve values corresponding to the specified list of keys. 
}
\usage{
redisMGet(keys)
}
\arguments{
  \item{keys}{A vector or list of character strings  corresponding to 
keys to retrieve.}
}
\value{
A list of retrieved key/value pairs. Missing values return NULL.
}
\details{
Values are returned in a list with names corresponding to keys.
}
\references{
http://code.google.com/p/redis/wiki/MGetCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisMSet}}
}
\examples{
\dontrun{
redisSet('x',runif(5))
redisSet('y',runif(5))
redisMGet(c('x','y'))
}
}
