\name{redisLPush}
\alias{redisLPush}
\alias{redisRPush}
\title{Add a value to the head tail of a list.
}
\description{
Add values to the tail (LPush) or head (RPush) of a list
corresponding to the specified key.
}
\usage{
redisLPush(key, value, ...)
}
\arguments{
  \item{key}{The desired key corresponding to a list.}
  \item{value}{The element to add to the list.}
  \item{...}{Optional additional values to add to the list.}
}
\details{
If the key does not exist an empty list is created just before the 
append operation. If the key exists but is not a list an error is returned.
}
\value{
The length of the list after the push operation.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisRPush}}
}
\examples{
\dontrun{
redisConnect()
redisLPush('x',1)
redisLPush('x',2)
redisLPush('x',3)
redisRPush('x',11)
redisRPush('x',21)
redisRPush('x',31)
redisLLen('x')
}
}
