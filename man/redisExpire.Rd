\name{redisExpire}
\alias{redisExpire}
\title{Set a timeout on the specified key.}
\description{
Set a timeout on the specified key, after which the key and
corresponding value will be deleted.
}
\usage{
redisExpire(key, seconds)
}
\arguments{
  \item{key}{The character key on which to set the timeout.}
  \item{seconds}{The integer timeout in seconds.}
}
\details{Operations that modify value(s) corresponding to a key subsequent
to the \code{redisExpire} function clear the timeout, removing the expiration.
The \code{redisExpire} function can't set a new timeout value once a timeout
has been set on a key.
}
\value{
Boolean TRUE if the timeout command was successful, FALSE otherwise.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisExpireAt}}
}
\examples{
\dontrun{
redisConnect()
redisLPush('x',runif(5))
redisExpire('x', 3)
}
}
