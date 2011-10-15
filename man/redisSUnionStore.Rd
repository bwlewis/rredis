\name{redisSUnionStore}
\alias{redisSUnionStore}
\title{
Store the union of two or more sets.
}
\description{
Store the union of two or more sets in the specified set.
}
\usage{
redisSUnionStore(dest, keys, ...)
}
\arguments{
  \item{dest}{
The destination set in which to store the result.
}
  \item{keys}{
A vector or list of keys corresponding to sets. May also be a single
key.
}
  \item{...}{
Additional keys corresponding to sets. See the examples below.
}
}
\details{
The \code{keys} argument may be a vector of set names, a list of set names,
or a single set name. If only a single set name is specified, specify
more sets as additional function arguments as shown in the examples.
}
\value{
A redis status code.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSInterStore}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd('A',1)
redisSAdd('A',2)
redisSAdd('A',3)
redisSAdd('B',2)
redisSUnionStore('C','A','B')
}
}
