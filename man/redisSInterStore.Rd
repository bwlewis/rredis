\name{redisSInterStore}
\alias{redisSInterStore}
\title{
Store intersection of two or more sets.
}
\description{
Find the intersection of two or more sets, storing the result in the
specified set.
}
\usage{
redisSInterStore(dest, keys, ...)
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
http://code.google.com/p/redis/wiki/SinterstoreCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSUnionStore}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd('A',1)
redisSAdd('A',2)
redisSAdd('A',3)
redisSAdd('B',2)
redisSInterStore('C','A','B')
}
}
