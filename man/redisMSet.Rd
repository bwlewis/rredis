\name{redisMSet}
\alias{redisMSet}
\title{Set one or more key/value pairs in the Redis database.}
\description{Set one or more key/value pairs in the Redis database.}
\usage{
redisMSet(keyvalues, NX = FALSE)
}
\arguments{
  \item{keyvalues}{A list of values to set of the form 
list(key1=value1, key2=value2, ...)}
  \item{NX}{
If NX = TRUE, existing values will not be replaced, otherwise they will be.
}
}
\details{
Set one or more key/value pairs in the Redis database. Existing
values will be replaced.
}
\value{
``OK'' upon success. 
}
\references{
http://code.google.com/p/redis/wiki/MSetCommand
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisGet}}
}
\examples{
\dontrun{
redisMSet(list(x=5, y=6))
}
}
