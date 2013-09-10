\name{redisSetBit}
\alias{redisSetBit}
\title{Redis BITSET - set binary value}
\usage{
  redisSetBit(key, offset, bit)
}
\arguments{
  \item{key}{redis key}

  \item{offset}{integer index to be updated}

  \item{bit}{binary number to be set}
}
\value{
  bit binary number with previous value, or '0' if it had
  not been set before.
}
\description{
  Sets or clears the bit at offset in the string value
  stored at key.
}

