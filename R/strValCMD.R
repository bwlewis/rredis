# This file contains functions that operate on Redis 'string' values.

redisGet <- function(key) {
  .sendCmd(.redismsg('GET',key))
}

# This is only useful right now because it is faster than mset.
# We could probably roll them together, but I'm not sure if that
# will be a pain later. -PS
redisSet <- function(key, value, NX=FALSE) {
  value <- .cerealize(value)
  if (NX) cmd <- 'SETNX ' else cmd <- 'SET '
  msg <- paste(cmd,key,' ',length(value),'\r\n',sep='')
  ret <- .sendCmd(msg,value)
  if (NX) 1==ret else ret
}

redisGetSet <- function(key, value) {
  value <- .cerealize(value)
  msg <- paste('GETSET ',key,' ',length(value),'\r\n',sep='')
  .sendCmd(msg,value)
}

redisMGet <- function(keys) {
  .sendCmd(.redismsg('MGET',paste(keys,collapse=' ')),names=keys)
}

redisMSet <- function(keyvalues, NX=FALSE) {
  if (NX) cmd <- 'MSETNX' else cmd <- 'MSET'
  cmd <- c(list(charToRaw(cmd)),keyvalues)
  ret <- .sendCmdMulti(cmd)
  if (NX) 1==ret else ret
}

redisIncr <- function(key)
{
  .sendCmd(.redismsg('INCR',key))
}

redisIncrBy <- function(key, value)
{
  .sendCmd(.redismsg('INCRBY',key,value))
}

redisDecrBy <- function(key, value)
{
  .sendCmd(.redismsg('DECRBY',key,value))
}

redisDecr <- function(key)
{
  .sendCmd(.redismsg('DECR',key))
}
