\name{redisExpireAt}
\alias{redisExpireAt}
\title{Set a timeout on the specified key.}
\description{
Set a timeout on the specified key, after which time the key and
corresponding value will be deleted.
}
\usage{
redisExpireAt(key, time)
}
\arguments{
  \item{key}{The character key on which to set the timeout.}
  \item{time}{The UNIX time of expiration.}
}
\details{Operations that modify value(s) corresponding to a key subsequent
to the \code{redisExpireAt} function clear the timeout, removing the expiration.
The \code{redisExpireAt} function can't set a new timeout value once a timeout
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
\code{\link{redisExpire}}
}
\examples{
\dontrun{
redisConnect()
redisLPush('x',runif(5))
redisExpireAt('x', 1266209144)
}
}
