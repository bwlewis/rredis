\name{redisZInterStore}
\alias{redisZInterStore}
\title{
redisZInterStore
}
\description{
Store the intersection of two sorted sets into another sorted set.
}
\usage{
redisZInterStore(dstkey, keys, weights = c(), aggregate = NULL)
}
\arguments{
  \item{dstkey}{
The destination set name.
}
  \item{keys}{
A vector or list of sorted set names.
}
  \item{weights}{
A set of weights equal in length to the \code{keys} argument, or NULL (default). If specified, the weights associated with the members of each set will be multiplied by the specified weight values.
}
  \item{aggregate}{
A character value of either "SUM" "MIN" or "MAX" indicating how the resulting weights should be aggregated.
}
}
\details{
See the Redis documentation for details.
}
\value{
The number of elements stored in the destination set.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
