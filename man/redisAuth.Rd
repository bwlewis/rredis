\name{redisAuth}
\alias{redisAuth}
\title{Redis authentication.}
\description{Redis supports a trivially simple and insecure authentication 
method. This function implements it.
}
\usage{
redisAuth(pwd)
}
\arguments{
  \item{pwd}{
The (required) password.
}
}
\details{You should not use this function. If you need a secure key/value
database, it's best not to use Redis for now.
}
\value{
TRUE if sueccessful, FALSE otherwise.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
