# This file contains functions that operate on Redis 'string' values.

redisGet <- function(key, ...)
{
  .redisCmd(.raw('GET'), .raw(key), ...)
}

redisSet <- function(key, value, NX=FALSE)
{
  value <- .cerealize(value)
  cmd <- 'SET'
  if(NX) cmd <- 'SETNX'
  .redisCmd(.raw(cmd), .raw(key), value)
}

redisGetSet <- function(key, value, ...)
{
  .redisCmd(.raw('GETSET'),.raw(key),value, ...)
}

redisMGet <- function(keys, ...)
{
  keylist <- as.list(keys)
  x <- do.call('.redisCmd',c(lapply(c(list('MGET'),keylist),charToRaw),...))
# The following may not occur, for example within a transaction block:
  if(length(x) == length(keylist)) names(x) <- keylist
  x
}

redisMSet <- function(keyvalues, NX=FALSE, ...)
{
# Includes a significant performance improvement contributed
# by William Pleasant.
  if (0L == length(keyvalues))
      return(NULL)
  if (NX) cmd <- 'MSETNX' else cmd <- 'MSET'
  a <- list(.raw(cmd))
  j <- 2L * seq_along(keyvalues)
  a[j + 1L] <- keyvalues                # extends a
  a[j] <- lapply(names(keyvalues), charToRaw)
  a <- c(a, ...)
  do.call('.redisCmd', a)
}

redisIncr <- function(key)
{
  .redisCmd(.raw('INCR'),.raw(key))
}

redisIncrBy <- function(key, value)
{
  .redisCmd(.raw('INCRBY'),.raw(key),.raw(as.character(value)))
}

redisIncrByFloat <- function(key, value)
{
  .redisCmd(.raw('INCRBYFLOAT'),.raw(key),.raw(as.character(value)))
}

redisDecrBy <- function(key, value)
{
  .redisCmd(.raw('DECRBY'),.raw(key),.raw(as.character(value)))
}

redisDecr <- function(key)
{
  .redisCmd(.raw('DECR'),.raw(key))
}
