\name{redisZRange}
\alias{redisZRange}
\title{
redisZRange
}
\description{
Return a subset of an ordered set.
}
\usage{
redisZRange(key, start = 0, end = -1, decreasing = FALSE, withscores = FALSE, ...)
}
\arguments{
  \item{key}{
The set name.
}
  \item{start}{
Starting index value.
}
  \item{end}{
Ending index value.
}
  \item{decreasing}{
Set TRUE to sort in decreasing order of scores.
}
  \item{withscores}{
If TRUE, also return set element scores.
}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\details{
Return the specified elements of the sorted set at the specified key. The
elements are considered sorted from the lowerest to the highest score
if decreasing = FALSE, and from highest to lowest score 
if decreasing = TRUE. Start and end are
zero-based indexes. 0 is the first element of the sorted set (the one with the
lowerest score), 1 the next element by score and so on.
start and end can also be negative numbers indicating offsets from the end of
the sorted set. For example -1 is the last element of the sorted set, -2 the
penultimate element and so on.  

Indexes out of range will not produce an error:
if start is over the end of the sorted set, or start is greater than end, 
an empty list is
returned. If end is over the end of the sorted set Redis will threat it just
like the last element of the sorted set.
}
\value{
A list of returned set elements. If withscores = TRUE, two lists called
'elements' and 'scores' are returned.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
