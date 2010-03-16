\name{redisSRem}
\alias{redisSRem}
\title{Remove an element from a set.}
\description{
Remove an element from a set.
}
\usage{
redisSRem(set, element)
}
\arguments{
  \item{set}{The set name (character) from which to remove the element.}
  \item{element}{The element to remove (not and index, but the actual element
value).}
}
\details{
The set element matching the specified element will be removed from the set.
}
\value{
TRUE upon successful removal, FALSE otherwise.
}
\references{
http://code.google.com/p/redis/wiki/SremCommand
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
redisSAdd('set', 5)
redisSAdd('set', 7)
redisSMembers('set')
redisSRem('set',5)
redisSMembers('set')
}
}
