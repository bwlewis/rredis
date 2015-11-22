\name{Hyperloglog functions}
\alias{redisPfadd}
\alias{redisPfcount}
\alias{redisPfmerge}
\title{Redis HyperLogLog Functions}
\description{
\code{redisPfadd}
Adds all the element arguments to the HyperLogLog data structure stored at the
key name specified as first argument.
\code{redisPfcount}

When called with a single key, returns the approximated cardinality computed by
the HyperLogLog data structure stored at the specified variable, which is 0 if
the variable does not exist.

When called with multiple keys, returns the approximated cardinality of the
union of the HyperLogLogs passed, by internally merging the HyperLogLogs stored
at the provided keys into a temporary HyperLogLog.

\code{redisPfmerge}
Merge multiple HyperLogLog values into a unique value that will approximate
the cardinality of the union of the observed Sets of the source HyperLogLog
structures.

}
\usage{
redisPfadd(key, elements)
redisPfcount(keys)
redisPfmerge(destkey, sourcekeys)
}
\arguments{
  \item{key}{A Redis key name.}
  \item{elements}{A vector or list of elements to add to the HyperLogLog data structure associated with \code{key}.}
  \item{destkey}{The computed merged HyperLogLog is set to the destination key, which is
created if does not exist (defaulting to an empty HyperLogLog).}
  \item{sourcekeys}{A single key name or vector of Redis key names corresponding to HyperLogLog values to be
merged into a unqiue value.}
}
\details{
The HyperLogLog data structure can be used in order to count unique elements in
a set using just a small constant amount of memory, specifically 12k bytes for
every HyperLogLog (plus a few bytes for the key itself).

The returned cardinality of the observed set is not exact, but approximated
with a standard error of 0.81\%.  For example in order to take the count of all
the unique search queries performed in a day, a program needs to call
\code{redisPfadd} every time a query is processed. The estimated number of
unique queries can be retrieved with PFCOUNT at any time.
}
\value{
\code{redisPfadd} returns "1" if at least 1 HyperLogLog internal register was altered, "0" otherwise.

\code{redisPfcount} returns a Redis integer value as a character string
corresponding to the approximated number of unique elements observed.

\code{redisPfmerge} just returns "OK".
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

\seealso{
\code{\link{redisPfadd}}
\code{\link{redisPfcount}}
\code{\link{redisPfmerge}}
}
\examples{
\dontrun{
redisPfadd("testcounter", letters[1:10])
redisPfadd("testcounter", letters[1:20])
redisPfcount("testcounter")

redisPfadd("testcounter_2", "z")
redisPfmerge("testcounter", "testcounter_2")
redisPfcount("testcounter")
}
}
