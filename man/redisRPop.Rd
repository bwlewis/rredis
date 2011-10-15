\name{redisRPop}
\alias{redisRPop}
\title{Remove the last element from a list.}
\description{
Atomically return and remove the last element of the list associated with
the specified key. 
For example if the list contains the elements "a","b","c" 
\code{redisRPop} will return "c" and the list will become "a","b".
}
\usage{
redisRPop(key)
}
\arguments{
  \item{key}{The desired key associated with a list.}
}
\value{The first element of the list associated with the specified key,
or NULL if the list is empty.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisLPop}}, \code{\link{redisLPush}}
}
\examples{
\dontrun{
redisConnect()
redisLPush('a',1)
redisLPush('a',2)
redisLPush('a',3)
redisRPop('a')
}
}
