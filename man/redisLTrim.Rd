\name{redisLTrim}
\alias{redisLTrim}
\title{Trim a list.}
\description{
Trim an existing list so that it will contain only the specified range of
elements specified.
}
\usage{
redisLTrim(key, start, end)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{A key corresponding to a Redis list object.
}
  \item{start}{The starting list index at which to trim.
}
  \item{end}{The ending list index at which to trim.
}
}
\details{
Start and end are zero-based indexes. 0 is the first
element of the list (the list head), 1 the next element and so on.
}
\value{
TRUE if successful. An error is thrown if the key does not exist or
correspond to a list value.
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
redisLTrim('x',0,1)
redisLRange('x',0,99)
}
}
