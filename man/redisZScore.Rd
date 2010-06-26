\name{redisZScore}
\alias{redisZScore}
\title{
redisZScore
}
\description{
Return the score of an ordered set element.
}
\usage{
redisZScore(key, element)
}
\arguments{
  \item{key}{
The set name.
}
  \item{element}{
The set element.
}
}
\value{
The numeric score.
}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
