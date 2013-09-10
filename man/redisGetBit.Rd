\name{redisGetBit}
\alias{redisGetBit}
\title{Redis BITSET gets - get binary value}
\usage{
  redisGetBit(key, offset, ...)
}
\arguments{
  \item{key}{redis key}

  \item{offset}{integer index}
  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\value{
  bit binary integer
}
\description{
  Returns the bit value at offset in the string value
  stored at key.
}

