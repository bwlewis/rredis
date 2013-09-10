\name{redisLRange}
\alias{redisLRange}
\title{Copy values from a list.}
\description{Return the elements of a list between the start and end
indices, inclusively.
}
\usage{
redisLRange(key, start, end, ...)
}
\arguments{
  \item{key}{A key corresponding to a list.  }
  \item{start}{The beginning index from which to read list elements.  }
  \item{end}{The ending index.}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\details{
List indices begin with 0. If the start or end indices are beyond the bounds
of the list, return the list elements up to the index bounds.
}
\value{An indexed list containing the returned values. An error will be
thrown if the specified key does not correspond to a list or if the key
can't be found.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisLPop}}
}
\examples{
\dontrun{
redisConnect()
redisLPush('x',1)
redisLPush('x',2)
redisLPush('x',3)
redisLRange('x',0,2)
}
}
