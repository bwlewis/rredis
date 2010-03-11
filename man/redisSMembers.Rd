\name{redisSMembers}
\alias{redisSMembers}
\title{List elements of a set.}
\description{Return a list containing all the members (elements) 
of the specified set.
}
\usage{
redisSMembers(set)
}
\arguments{
  \item{set}{The set name (character).} }
\details{ }
\value{
A list containing the set elements. An error is thrown if the set does
not exist.
}
\references{
http://code.google.com/p/redis/wiki/SmembersCommand
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
