\name{redisSort}
\alias{redisSort}
\title{
redisSort
}
\description{
Sort a list, set or sorted set (zset).
}
\usage{
redisSort(key, decreasing = FALSE, alpha = FALSE, by = NULL, start = NULL, count = NULL, get = NULL, store = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{
The key name of the list, set or zset to be sorted.
}
  \item{decreasing}{
Set the direction of the sort.
}
  \item{alpha}{
Lexicographically sort if true, otherwise try numeric sorting.
}
  \item{by}{
The BY option takes a pattern that is used in order
to generate the key names of the weights used for sorting. Weight key names are
obtained substituting the first occurrence of * with the actual value of the
elements on the list. See the Redis documentation for examples.
}
  \item{start}{
Starting index of the sort.
}
  \item{count}{
Number of entries past start to use for the sort.
}
  \item{get}{
Retrieve external keys. See the Redis documentation.
}
  \item{store}{
Store the results in the specified key.
}
}
\details{
Sort the elements contained in the List, Set, or Sorted Set value at key. By
default sorting is numeric with elements being compared as double precision
floating point numbers.
}
\value{
A list of sorted values, unless \code{store} is specified in which case 
the results are stored in the specified key and TRUE is returned.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
