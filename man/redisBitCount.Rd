\name{redisBitCount}
\alias{redisBitCount}
\title{Redis BITCOUNT - count all bits in key}
\usage{
  redisBitCount(key)
}
\arguments{
  \item{key}{redis key}
}
\value{
  the counted bits as an integer value
}
\description{
  Count the number of set bits (population counting) in a
  string.
}

