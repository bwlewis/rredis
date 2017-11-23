\name{redisKeys}
\alias{redisKeys}
\title{Return a list of all keys in the active Redis database.}
\description{
Return a list of all keys in the active Redis database that match
the specified pattern.
}
\usage{
redisKeys(pattern = "*")
}
\arguments{
  \item{pattern}{The character string pattern used to match keys.}
}
\details{Basic string-matching wildcards are supported. Use '?' to match
any single character and '*' to match zero or more characters.
}
\value{A space-delimited character string containing all keys that match
the pattern in the active Redis database.
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
redisKeys()
}
}
