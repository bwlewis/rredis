\name{redisLPop}
\alias{redisLPop}
\title{Remove the first element from a list.}
\description{
Atomically return and remove the first element of the list. 
For example if the list contains the elements "a","b","c" 
\code{redisLPop} will return "a" and the list will become "b","c".
}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\usage{
redisLPop(key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{The desired key associated with a list.}
}
\value{The first element of the list associated with the specified key,
or NULL if the list is empty.
}
\references{
http://code.google.com/p/redis/wiki/LpopCommand
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisRPop}}, \code{\link{redisLPush}}
}
\examples{
\dontrun{
redisConnect()
redisLPush('a',1)
redisLPush('a',2)
redisLPush('a',3)
redisLPop('a')
}
}
