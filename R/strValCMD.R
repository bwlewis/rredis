# This file contains functions that operate on Redis 'string' values.

redisGet <- function(key) {
  msg <- paste('GET ',key,'\r\n',sep='')
  .sendCmd(msg)
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
  msg <- paste('MGET ',paste(keys, collapse=' '),'\r\n',sep='')
  .sendCmd(msg,names=keys)
}

redisMSet <- function(keyvalues, NX=FALSE) {
  if (NX) cmd <- 'MSETNX' else cmd <- 'MSET'
  cmd <- c(list(charToRaw(cmd)),keyvalues)
  ret <- .sendCmdMulti(cmd)
  if (NX) 1==ret else ret
}

redisIncr <- function(key)
{
  msg <- paste('INCR ',key,'\r\n',sep='')
  .sendCmd(msg)
}

redisIncrBy <- function(key, value)
{
# XXX Add check for integer value
  msg <- paste('INCRBY ',key,' ',value,'\r\n',sep='')
  .sendCmd(msg)
}

redisDecrBy <- function(key, value)
{
# XXX Add check for integer value
  msg <- paste('DECRBY ',key,' ',value,'\r\n',sep='')
  .sendCmd(msg)
}

redisDecr <- function(key)
{
  msg <- paste('DECR ',key,'\r\n',sep='')
  .sendCmd(msg)
}
