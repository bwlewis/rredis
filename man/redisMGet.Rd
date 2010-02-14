\name{redisMGet}
\alias{redisMGet}
\title{Retrieve one or more values from Redis.}
\description{Retrieve values corresponding to the specified list of keys. 
}
\usage{
redisMGet(keys)
}
\arguments{
  \item{keys}{A character vector or list of keys to retrieve.}
}
\details{
}
\value{
A list of retrieved key/value pairs.
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
redisStore('x',runif(5))
redisStore('y',runif(5))
redisMGet(c('x','y'))
}
}
