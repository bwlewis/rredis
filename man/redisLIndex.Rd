\name{redisLIndex}
\alias{redisLIndex}
\title{Retrieve a value from a Redis 'list.'}
\description{
Retrieve a value from a Redis 'list' at the specified index without 
removing it from the list.
}
\usage{
redisLIndex(key, index)
}
\arguments{
  \item{key}{The key (whose value is of the type 'list').}
  \item{index}{The list index to retrieve.}
}
\details{
List indices begin at 0.
Negative indexes are supported, for example -1 is the last element, 
-2 the penultimate and so on.
If the value stored at key is not of the 'list' type an error is returned. 
If the index is out of range an empty string is returned.
}
\value{
The corresponding value or an empty string if the index is out of bounds.
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
}
}
