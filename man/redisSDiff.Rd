\name{redisSDiff}
\alias{redisSDiff}
\title{
Return the difference of two or more sets.
}
\description{
Return the (set) difference of two or more sets.
}
\usage{
redisSDiff(keys, ...)
}
\arguments{
  \item{keys}{
A vector or list of keys corresponding to sets. May also be a single
key.
}
  \item{...}{
Additional keys corresponding to sets. See the examples below.
}
}
\details{
The first argument may be a vector of set names, a list of set names,
or a single set name. If only a single set name is specified, specify
more sets as additional function arguments as shown in the examples.
}
\value{
A list of elements in the difference of the specified sets, or 
NULL if the intersection is the empty set.
}
\references{
http://code.google.com/p/redis/wiki/SdiffCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSDiff}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd('A',1)
redisSAdd('A',2)
redisSAdd('B',4)
redisSDiff('A','B')
redisSDiff(c('A','B'))
redisSDiff(list('A','B'))
}
}
