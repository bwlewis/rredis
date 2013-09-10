\name{redisBLPop}
\alias{redisBLPop}
\title{Blocking List Pop}
\description{Pop the first available value from a key or list of keys, 
blocking until a value is available.}
\usage{
redisBLPop(keys, timeout = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{keys}{A character key value or vector of key values to monitor.}
  \item{timeout}{
  A timeout in seconds after which, if no value is available, the function
returns NULL. A value of zero indicates block indefinitely.
}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\details{
redisBLPop blocks until at least one of the keys exists and contains a
non-empty value, or until the specified timeout period is reached, whichever
comes first. Keys are scanned in the order that they are specified.
}
\value{
redisBLPop returns NULL after the timeout period, or a list containing:
  \item{key}{The first key encountered with an available value,}
  \item{value}{The corresponding value.}
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{redisBRpop}
}
\examples{
\dontrun{
redisConnect()
redisBLpop('x', 5)
redisLPush('x',runif(5))
redisBLPop('x')
}
}
