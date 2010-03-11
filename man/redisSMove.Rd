\name{redisSMove}
\alias{redisSMove}
\title{Move a set element.}
\description{
Move the specified element from one set to another.
}
\usage{
redisSMove(setA, setB, element)
}
\arguments{
  \item{setA}{The set to move from (character identifier).}
  \item{setB}{The set to move to (character identifier).}
  \item{element}{The element to move.}
}
\details{Note that the set element is specified as the actual set element object, not an index value.
}
\value{1 if successful, 0 otherwise.
}
\references{
http://code.google.com/p/redis/wiki/SmoveCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSAdd}}
}
\examples{
}
