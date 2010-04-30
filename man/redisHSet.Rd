\name{redisHSet}
\alias{redisHSet}
\title{Store a hash value in Redis.}
\description{Store a value identified by a character key name and field
in the Redis database. Any existing value with the same key/field 
will be replaced by the new value unless NX is set to TRUE.
}
\usage{
redisHSet(key, field, value, NX = FALSE)
}
\arguments{
  \item{key}{
The (required) character identifier for the key name. 
}
  \item{field}{
The (required) character identifier for the field name. 
}
  \item{value}{
The (required) object to associate with the key/field.
}
  \item{NX}{
If NX = TRUE, existing values will not be replaced.
}
}
\details{
Redis hash values store values in one or more fields associated with a single
key name.

The value to be stored can be any
serializable R object up to the Redis maximum object size 
(excluding, for example, external pointer references).
References to other R objects or environments inside the value are not
guaranteed to be preserved.

In order to store strings that can easily be read by other clients, first
convert the character object using the 
\code{\link{charToRaw}} function as shown in the
examples.
}
\value{
TRUE is returned on success. 
If NX = FALSE and a value already exists, the value will not be replaced
and FALSE will be returned.
}
\references{
http://code.google.com/p/redis/wiki/HsetCommand
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisHGet}}
}
\examples{
\dontrun{
redisHSet('A', 'x',runif(5))
}
}
