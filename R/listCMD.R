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
  start <- charToRaw(as.character(start))
  end <- charToRaw(as.character(end))
  cmd <- list(LRANGE=charToRaw(key),start,end)
  .sendCmdMulti(cmd)
}

redisLTrim <- function(key,start,end) {
  start <- charToRaw(as.character(start))
  end <- charToRaw(as.character(end))
  cmd <- list(LTRIM=charToRaw(key),start,end)
  .sendCmdMulti(cmd) == "OK"
}

redisLIndex <- function(key, index) {
  .sendCmd(.redismsg('LINDEX', key, index))
}

redisLSet <- function(key, index, value) {
  key <- charToRaw(as.character(key))
  index <- charToRaw(as.character(index))
  cmd <- list(LSET=key,index,value)
  .sendCmdMulti(cmd) == "OK"
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
  x <- .sendCmd(.redismsg('BRPOP', paste(keys, collapse=' '), timeout))
  if(length(x)>1) {
    n <- x[[1]]
    x <- list(x[[2]])
    names(x) <- n
  }
  x
}

redisBLPop <- function(keys, timeout=0) {
  x <- .sendCmd(.redismsg('BLPOP', paste(keys, collapse=' '), timeout))
  if(length(x)>1) {
    n <- x[[1]]
    x <- list(x[[2]])
    names(x) <- n
  }
  x
}
