\name{redisZRangeByScore}
\alias{redisZRangeByScore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
redisZRangeByScore
}
\description{
Return the all the elements in the sorted set at key with a score between 
\code{min} and \code{max} (including elements with score equal to 
\code{min} or \code{max}).
}
\usage{
redisZRangeByScore(key, min, max, offset = NULL, count = NULL, withscores = FALSE, ...)
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
  \item{offset}{
Limit the results by excluding the first \code{offset} items. Requires
also the use of \code{count}.
}
  \item{count}{
Limit the results to the first \code{count} items. Requires also the use
of \code{offset}.
}
  \item{withscores}{
Also return the scores.
}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\details{
The \code{offset} and \code{count} arguments must both be specified if used.
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
