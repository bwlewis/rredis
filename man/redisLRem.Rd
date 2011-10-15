\name{redisLRem}
\alias{redisLRem}
\title{Remove elements from a list.}
\description{Remove the fist \code{count} occurences of the given value
from a list corresponding to the specified key.
}
\usage{
redisLRem(key, count, value)
}
\arguments{
  \item{key}{The desired key corresponding to a list.
}
  \item{count}{
The maximum number of occurences to remove.
}
  \item{value}{
Remove elements matching this value from the list.
}
}
\details{
Remove the first \code{count} occurrences of the \code{value} element from the
list. If \code{count} is zero all the elements are removed.  If count is
negative elements are removed from tail to head, instead of the default removal
behavior from head to tail.  So for example \code{redisLRem} with count -2 and
value "hello" applied to the list (a,b,c,hello,x,hello,hello) will
yield the list (a,b,c,hello,x). The number of removed elements is returned as
an integer upon success. Note that
non-existing keys are considered as empty lists.
}
\value{
The number of occurrences removed.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisLPush}}
}
\examples{
\dontrun{
redisConnect()
}
}
