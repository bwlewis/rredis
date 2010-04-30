\name{redisHMSet}
\alias{redisHMSet}
\title{Store a list of hash values.}
\description{Store a list of hash values at once.
}
\usage{
redisHMSet(key, values)
}
\arguments{
  \item{key}{
The (required) character identifier for the key name.
}
  \item{values}{
An R list of values to be stored. The list names are used as field names and
must be nonempty.
}
}
\details{
Redis hash values store values in one or more fields associated with a single
key name. The redisHMSet function stores several fields associated with one
key at once. Values already occupying any specified field slots are replaced.
}
\value{
TRUE is returned on success. 
}
\references{
http://code.google.com/p/redis/wiki/HmsetCommand
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisHGet}}
}
\examples{
\dontrun{
redisHMSet('A', list(x=1,y=2,z=3))
redisHGet('A','y')
}
}
