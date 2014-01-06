\name{redisZCount}
\alias{redisZCount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
redisZCount
}
\description{
Returns the number of elements in the sorted set at key with a score between min and max.
The min and max arguments have the same semantic as described for \code{redisZRangeByScore}.
}
\usage{
redisZCount(key, min, max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{
The set name.
}
  \item{min}{
Minimum score.
}
  \item{max}{
Maximum score.
}
}
\value{
See the Redis documentation for more information.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
