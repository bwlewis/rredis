\name{redisRandomKey}
\alias{redisRandomKey}
\title{
Return a randomly selected key from the currently selected database.
}
\description{
Return a randomly selected key from the currently selected database.
}
\usage{
redisRandomKey()
}
\details{
Note that this function returns key names, not values.
}
\value{
A character string corresponding to a randomly-selected key from the currently
selected database, or a zero-length string if no keys are defined.
}
\references{
http://code.google.com/p/redis/wiki/RandomkeyCommand
}
\author{
B. W. Lewis
}
\examples{
\dontrun{
redisConnect()
redisRandomKey()
}
}
