\name{redisRPush}
\alias{redisRPush}
\title{Add a value to the head of a list.
}
\description{
Add a value to the head of a list corresponding to the specified key.
}
\usage{
redisRPush(key, value)
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
The length of the list after the successfull push operation.
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
redisRPush('x',1)
redisRPush('x',2)
redisRPush('x',3)
}
}
