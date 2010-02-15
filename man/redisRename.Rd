\name{redisRename}
\alias{redisRename}
\title{
Rename a key.
}
\description{
Atomically rename a key. 
}
\usage{
redisRename(old, new, NX = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{old}{Original key name.}
  \item{new}{New key name.}
  \item{NX}{Set NX = TRUE to prevent overwriting a key that already exists.
}
}
\details{
If the source and destination name are the same an error is returned. If newkey already exists it is overwritten unless NX = TRUE, in which case the 
operation fails.
}
\value{
TRUE if successful, FALSE otherwise.
}
\references{
http://code.google.com/p/redis/wiki/RenameCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisKeys}}
}
\examples{
\dontrun{
redisConnect()
redisSet('x',5)
redisRename('x','y')
}
}
