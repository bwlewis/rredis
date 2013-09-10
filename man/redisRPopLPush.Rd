\name{redisRPopLPush}
\alias{redisRPopLPush}
\title{Remove the tail from a list, pushing to another.
}
\description{
Atomically return and remove the last (tail) element of the src list, and
push the element as the first (head) element of the dst list.
}
\usage{
redisRPopLPush(src, dest, ...)
}
\arguments{
  \item{src}{A key corresponding to the source list.}
  \item{dest}{A key corresponding to the destination list.}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\details{
Atomically return and remove the last (tail) element of the src list, and
push the element as the first (head) element of the dst list. For example if
the source list contains the elements "a","b","c" and the destination list
contains the elements "foo","bar" after a \code{redisRPopLPush} 
command the content of the
two lists will be "a","b" and "c","foo","bar".

If the key does not exist or the list is already empty the special value NULL 
is returned. If the srckey and dstkey are the same the operation is equivalent
to removing the last element from the list and pusing it as first element of
the list, so it's a "list rotation" command.

See the Redis reference below for programming examples and discussion.
}
\value{
The value moved or rotated across lists, or NULL if the source key does
not exist or corresponds to an empty list. An error is thrown if either
of the keys does not correspond to a value of 'list' type.
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
redisLPush('x',1)
redisLPush('x',2)
redisLPush('x',3)
redisRPopLPush('x','x')
redisRPopLPush('x','x')
}
}
