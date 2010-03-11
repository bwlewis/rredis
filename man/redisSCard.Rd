\name{redisSCard}
\alias{redisSCard}
\title{Set cardinality}
\description{
Return the number of elements contained in the specified set.
}
\usage{
redisSCard(set)
}
\arguments{
  \item{set}{
A string set identifier.
}
}
\value{
The function returns an integer corresponding to the number of elements
in the set. Zero is returned if the set is empty or if the set does not exist.
}
\references{
http://code.google.com/p/redis/wiki/ScardCommand
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
redisSAdd("set",5)
redisSCard("set")
}
}
