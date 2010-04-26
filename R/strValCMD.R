# This file contains functions that operate on Redis 'string' values.

redisGet <- function(key) {
  .redisCmd(.raw('GET'), .raw(key))
}

redisSet <- function(key, value, NX=FALSE) {
  value <- .cerealize(value)
  cmd <- 'SET'
  if(NX) cmd <- 'SETNX'
  retval <- .redisCmd(.raw(cmd), .raw(key), value)
  if(NX) 1 == retval
  else 'OK' == retval
}

redisGetSet <- function(key, value) {
  .redisCmd(.raw('GETSET'),.raw(key),value)
}

redisMGet <- function(keys) {
  keylist <- as.list(keys)
  x <- do.call('.redisCmd',lapply(c(list('MGET'),keylist),charToRaw))
  names(x) <- keylist
  x
}

redisMSet <- function(keyvalues, NX=FALSE) {
  if (NX) cmd <- 'MSETNX' else cmd <- 'MSET'
  a <- c(alist(),list(.raw(cmd)))
  rawnames <- lapply(as.list(names(keyvalues)),charToRaw)
  for(j in 1:length(keyvalues)) 
    a <- c(a,list(rawnames[[j]],keyvalues[[j]]))
  retval <- do.call('.redisCmd', a)
  if(NX) 1 == retval
  else 'OK' == retval
}

redisIncr <- function(key)
{
  .redisCmd(.raw('INCR'),.raw(key))
}

redisIncrBy <- function(key, value)
{
  .redisCmd(.raw('INCRBY'),.raw(key),.raw(as.character(value)))
}

redisDecrBy <- function(key, value)
{
  .redisCmd(.raw('DECRBY'),.raw(key),.raw(as.character(value)))
}

redisDecr <- function(key)
{
  .redisCmd(.raw('DECR'),.raw(key))
}
