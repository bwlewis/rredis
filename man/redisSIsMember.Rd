\name{redisSIsMember}
\alias{redisSIsMember}
\title{Test for set membership}
\description{
Test if an element belongs to a set.
}
\usage{
redisSIsMember(set, element)
}
\arguments{
  \item{set}{
A string set identifier.
}
  \item{element}{
The element whose membership is to be tested.
}
}
\value{
TRUE if the element is a member of the set, FALSE otherwise.
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
redisSAdd("set",5)
redisSIsMember("set",5)
}
}
