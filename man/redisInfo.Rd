\name{redisInfo}
\alias{redisInfo}
\title{redisInfo}
\description{Return system information about Redis.
}
\usage{
redisInfo()
}
\value{
A list of various Redis system parameters is returned, including at least:
the Redis version,
connected clients, 
connected slaves, 
used memory, 
changes since last save, 
last save time (UNIX time), 
total connections received, 
total commands processed, 
uptime in seconds, 
uptime in days.
}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
