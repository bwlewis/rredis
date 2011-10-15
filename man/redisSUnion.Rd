\name{redisSUnion}
\alias{redisSUnion}
\title{
Return the union of two or more sets.
}
\description{
Return the union of two or more sets.
}
\usage{
redisSUnion(keys, ...)
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
A list of elements in the union of the specified sets, or 
NULL if the intersection is the empty set.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSInter}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd('A',1)
redisSAdd('A',2)
redisSAdd('B',4)
redisSUnion('A','B')
redisSUnion(c('A','B'))
redisSUnion(list('A','B'))
}
}
