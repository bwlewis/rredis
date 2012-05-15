\name{redisCmd}
\alias{redisCmd}
\title{General Redis Interface Function}
\description{Perform any Redis command.}
\usage{
redisCmd(CMD, ..., raw = FALSE)
}
\arguments{
  \item{CMD}{
The (required) Redis command (character string).
}
  \item{...}{
Redis command arguments.
}
  \item{raw}{If TRUE, return results in raw byte form.}
}
\details{
Use this low-level function to perform any Redis operation.
The ... argument(s) not already in raw format will be
converted to raw byte format, and non-character values
will be serialized.
}
\value{
Output from the Redis command--varies depending on command.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\examples{
\dontrun{
redisCmd('set','x',runif(5))
redisCmd('get','x')
}
}
