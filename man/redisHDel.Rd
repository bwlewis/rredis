\name{redisHDel}
\alias{redisHDel}
\title{Delete a hash value.}
\description{
Delete the value associated with the given key/field combination.
}
\usage{
redisHDel(key, field)
}
\arguments{
  \item{key}{ A key name.  }
  \item{field}{ A field name.  }
}
\value{
Nothing is returned if the key/value pair is successfully deleted.
A warning is thrown if the they key could not be found.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisHSet}}
}
\examples{
\dontrun{
redisHMSet('A',list(x=1,y=2,z=3))
redisHDel('A','x')
}
}
