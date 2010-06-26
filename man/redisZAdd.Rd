\name{redisZAdd}
\alias{redisZAdd}
\title{
redisZAdd
}
\description{
Add an element to a Redis sorted set. Sorted sets order their elements
by numeric weights.
}
\usage{
redisZAdd(key, score, member)
}
\arguments{
  \item{key}{
The set name.
}
  \item{score}{
The numeric score associated with the new element (member).
}
  \item{member}{
The new object to add to the set.
}
}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
