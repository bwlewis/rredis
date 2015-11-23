\name{redisBitOp}
\alias{redisBitOp}
\title{Redis BITOP - execute bitoperations on multiple bitsets}
\usage{
  redisBitOp(operation, destkey, sourcekeys, ...)
}
\arguments{
  \item{operation}{bit operation as character: 'AND', 'OR',
  'XOR', 'NOT'}

  \item{destkey}{destination key where the resulting bit
  operation will be stored}

  \item{sourcekeys}{one or more source keys subject to the
  bit operations}

  \item{...}{ Optional additional arguments.  Specify \code{raw=TRUE} to skip de-serialization of the data.}
}
\value{
  the counted bits as an integer value
}
\description{
  Perform a bitwise operation between multiple keys
  (containing string values) and store the result in the
  destination key
}
