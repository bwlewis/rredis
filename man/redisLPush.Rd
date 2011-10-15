\name{redisLPush}
\alias{redisLPush}
\title{Add a value to the tail of a list.
}
\description{
Add a value to the tail of a list corresponding to the specified key.
}
\usage{
redisLPush(key, value)
}
\arguments{
  \item{key}{The desired key corresponding to a list.}
  \item{value}{The element to add to the list.
}
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
}
}
