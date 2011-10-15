\name{redisZRank}
\alias{redisZRank}
\title{
redisZRank
}
\description{
Return the rank of an element of an ordered set.
}
\usage{
redisZRank(key, member, decreasing = FALSE)
}
\arguments{
  \item{key}{
The set name.
}
  \item{member}{
The set element.
}
  \item{decreasing}{
Set to TRUE to rank in reverse order of score.
}
}
\details{
Returns the rank of the member in the sorted set, with scores ordered from 
low to high by default, or from high to low if decreasing = TRUE. 
The returned rank (or index) of the member is 0-based.
}
\value{
The numeric rank of the set element.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
