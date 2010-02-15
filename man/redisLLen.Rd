\name{redisLLen}
\alias{redisLLen}
\title{Redis list length.}
\description{Return the length of the Redis list associated with the 
specified key.}
\usage{
redisLLen(key)
}
\arguments{
  \item{key}{The key to look up.}
}
\details{Returns the length of the Redis list associated with \code{key}.
If the key is not found, or if the list is empty, 0 is returned. If the
key is associated with a value of type other than 'list,' an error is
thrown.
}
\value{The length if the list.
}
\references{
http://code.google.com/p/redis/wiki/LlenCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisBRpop}}
}
\examples{
\dontrun{
redisConnect()
redisLPush('list',1)
redisLPush('list',2)
redisLPush('list',3)
redisLLen('list')
}
}
