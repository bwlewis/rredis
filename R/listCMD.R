# This file contains functions that operate on Redis lists.

redisRPush <- function(key, value) {
  value <- .cerealize(value)
  .sendCmd(.redismsg('RPUSH',key,length(value)), value)
}

redisLPush <- function(key, value) {
  value <- .cerealize(value)
  .sendCmd(.redismsg('LPUSH',key,length(value)), value)
}

redisLLen <- function(key) {
  .sendCmd(.redismsg('LLEN',key))
}

redisLRange <- function(key, start, end) {
  .sendCmd(.redismsg('LRANGE', key, start, end))
}

redisLTrim <- function(key,start,end) {
  .sendCmd(.redismsg('LTRIM', key, start, end))
}

redisLIndex <- function(key, index) {
  .sendCmd(.redismsg('LINDEX', key, index))
}

redisLSet <- function(key, index, value) {
  .sendCmd(.redismsg('LSET', key, index, value))
}

redisLRem <- function(key, count, value) {
  .sendCmd(.redismsg('LREM', key, count, value))
}

redisRPop <- function(key) {
  .sendCmd(.redismsg('RPOP', key))
}

redisLPop <- function(key) {
  .sendCmd(.redismsg('LPOP', key))
}

redisRPopLPush <- function(src, dest) {
  .sendCmd(.redismsg('RPOPLPUSH',src,dest))
}

redisBRPop <- function(keys, timeout=0) {
  .sendCmd(.redismsg('BRPOP', paste(keys, collapse=' '), timeout))
}

redisBLPop <- function(keys, timeout=0) {
  .sendCmd(.redismsg('BLPOP', paste(keys, collapse=' '), timeout))
}
