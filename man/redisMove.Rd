\name{redisMove}
\alias{redisMove}
\title{
Move the specified key/value pair to another database.
}
\description{
Move the specified key/value pair in the currently selected database to
another database.
}
\usage{
redisMove(key, dbindex)
}
\arguments{
  \item{key}{The key to move.}
  \item{dbindex}{The destination database index number.}
}
\details{
This command returns TRUE only if the key was successfully moved, 
and FALSE if the target key was already there or if the source key was 
not found at all. It is possible to use \code{redisMove} as 
a locking primitive.
}
\value{
Returns TRUE if the key/value pair was moved, or FALSE otherwise.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSelect}}
}
\examples{
\dontrun{
redisConnect()
redisSelect(1)
redisSet('x',5)
redisMove('x',2)
redisSelect(2)
redisGet('x')
}
}
