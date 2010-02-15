\name{redisDBSize}
\alias{redisDBSize}
\title{Return the number of keys in the current Redis database.}
\description{
Return the number of keys in the current Redis database.
}
\usage{
redisDBSize()
}
\details{
Use \code{redisSelect} to choose a current database from 
among the available  Redis databases.
}
\value{
The number of keys in the current database.
}
\references{
http://code.google.com/p/redis/wiki/DbsizeCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSelect}}
}
\examples{
\dontrun{
redisDBSize()
}
}

