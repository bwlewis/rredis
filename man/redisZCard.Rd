\name{redisZCard}
\alias{redisZCard}
\title{
redisZCard.Rd
}
\description{
Report the number of elements in a sorted set.
}
\usage{
redisZCard(key)
}
\arguments{
  \item{key}{
The name of the set.
}
}
\details{
See the Redis documentation for details.
}
\value{
The number of elements in the set or zero if the set does not exist.
An error is thrown if the key name exists but is not a set.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
