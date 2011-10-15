\name{redisZRem}
\alias{redisZRem}
\title{
redisZRem
}
\description{
Delete an element from an ordered set.
}
\usage{
redisZRem(key, member)
}
\arguments{
  \item{key}{
The set name.
}
  \item{member}{
The element to delete.
}
}
\details{
See the Redis documentation for details.
}
\value{
0 or 1: 0 indicates that the element could not be removed, 1 that it was removed.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
