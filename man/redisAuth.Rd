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
\details{
If you use this function, it's probably a good idea to encrypt network
traffic between Redis and its client with a program like stunnel.
Otherwise, passwords are transmitted over the network in clear text.
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
