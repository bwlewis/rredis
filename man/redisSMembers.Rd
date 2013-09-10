\name{redisSMembers}
\alias{redisSMembers}
\title{List elements of a set.}
\description{Return a list containing all the members (elements) 
of the specified set.
}
\usage{
redisSMembers(set, ...)
}
\arguments{
  \item{set}{The set key name (character).}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\value{
A list containing the set elements. An error is thrown if the set does
not exist.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSAdd}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd('set',runif(5))
redisSMembers('set')
}
}
