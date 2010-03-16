\name{redisShutdown}
\alias{redisShutdown}
\title{redisShutdown}
\description{
Request the currently currently connected Redis server to shutdown, then
close the connection.
}
\usage{
redisShutdown()
}
\details{
This will terminate the connected Redis server process in an orderly way.
}
\value{
Nothing is returned. An error is thrown if no server is connected.
}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
