\name{redisSelect}
\alias{redisSelect}
\title{Select a redis database.}
\description{Select a database by numeric index value.}
\usage{
redisSelect(index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index}{The nonnegative integer value of the database to select.}
}
\details{Redis supports multiple databases indexed by nonnegative integers.
The number of supported databases is configurable via the redis server
configuration file. New connections are assigned to database 0 by default.
}
\value{A character string status code. An error is thrown if the index
value is invalid.}
\references{
http://code.google.com/p/redis/wiki/SelectCommand
}
\author{
B. W. Lewis
}
\examples{
\dontrun{
redisConnect()
redisSelect(1)
}
}
