\name{redisEval}
\alias{redisEval}
\title{Evaluate a Lua script in the Redis server.}
\description{Evaluate a Lua script in the Redis server.}
\usage{
redisEval(script, keys = vector("list",0), SHA = FALSE, ...)
}
\arguments{
  \item{script}{A Redis server-side Lua script (character).}
  \item{keys}{An optional list of script key arguments.}
  \item{SHA}{If TRUE, the script is a SHA1-encoded string, otherwise plan text.}
  \item{...}{Optional list of script arguments.}
}

\value{"OK" is returned on success.
Errors are displayed if the script fails.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{
redisConnect()
redisEval("return redis.call('set','foo','bar')")

# Supply a key argument to the script
redisEval("return redis.call('set',KEYS[1],'bar')", "foo")

# Supply a key and other arguments to the script
redisEval("return redis.call('set',KEYS[1],ARGV[1])", "foo", pi)
}
}
