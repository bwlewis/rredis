\name{redisSAdd}
\alias{redisSAdd}
\title{
Add an element to a set.
}
\description{
Add the element to the specified set.
}
\usage{
redisSAdd(set, element)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set}{
The string set identifier.
}
  \item{element}{
The object to add to the set.
}
}
\details{
 If member is already a member of the set no operation is performed. If key
does not exist a new set with the specified member as sole member is created.
If the key exists but does not hold a set value an error is returned.
}
\value{
1 if the element was successfully added, 0 otherwise (including cases in
which the element already belonged to the set).
}
\references{
http://code.google.com/p/redis/wiki/SaddCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSRem}}
}
\examples{
\dontrun{
redisConnect()
redisSAdd("set", 5)
}
}
