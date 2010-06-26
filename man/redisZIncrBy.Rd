\name{redisZIncrBy}
\alias{redisZIncrBy}
\title{
redisZIncrBy
}
\description{
Increment the weight associated with an ordered set element.
}
\usage{
redisZIncrBy(key, member, increment)
}
\arguments{
  \item{key}{
The set name.
}
  \item{member}{
The set element.
}
  \item{increment}{
The numeric value by which to increase the weight associated with the element.
}
}
\details{
See the Redis documentation for details.
}
\value{
The new weight associated with the element.
}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
