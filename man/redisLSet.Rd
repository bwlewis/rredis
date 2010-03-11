\name{redisLSet}
\alias{redisLSet}
\title{Set a value within a list.}
\description{Set the value of an element at the given index in a 
list corresponding  to the specified key.
}
\usage{
redisLSet(key, index, value)
}
\arguments{
  \item{key}{A key corresponding to a Redis list.
}
  \item{index}{The index within the list of the element to write to.
}
  \item{value}{The value to set.
}
}
\details{
Out of range indices throw an error.
}
\value{
}
\references{
http://code.google.com/p/redis/wiki/LSetCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisBRpop}}
}
\examples{
\dontrun{
redisConnect()
}
}
