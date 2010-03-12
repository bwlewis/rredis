\name{redisSInter}
\alias{redisSInter}
\title{
Find and return the intersection of two or more sets.
}
\description{
Find and return the intersection of two or more sets.
}
\usage{
redisSInter(keys, ...)
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
A list of elements in the intersection of the specified sets, or 
NULL if the intersection is the empty set.
}
\references{
http://code.google.com/p/redis/wiki/SinterCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSUnion}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd('A',1)
redisSAdd('A',2)
redisSAdd('A',3)
redisSAdd('B',2)
redisSInter('A','B')
redisSInter(c('A','B'))
redisSInter(list('A','B'))
}
}
