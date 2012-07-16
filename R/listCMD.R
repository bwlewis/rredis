# This file contains functions that operate on Redis lists.

redisRPush <- function(key, value) {
  .redisCmd(.raw('RPUSH'), .raw(key),value)
}

redisLPush <- function(key, value) {
  .redisCmd(.raw('LPUSH'), .raw(key),value)
}

redisRPop <- function(key) {
  .redisCmd(.raw('RPOP'), .raw(key))
}

redisLPop <- function(key) {
  .redisCmd(.raw('LPOP'), .raw(key))
}

redisLLen <- function(key) {
  .redisCmd(.raw('LLEN'), .raw(key))
}

redisLRange <- function(key, start, end) {
  start <- charToRaw(as.character(start))
  end <- charToRaw(as.character(end))
  .redisCmd(.raw('LRANGE'), .raw(key), start, end)
}

redisLTrim <- function(key,start,end) {
  start <- charToRaw(as.character(start))
  end <- charToRaw(as.character(end))
  .redisCmd(.raw('LTRIM'), .raw(key), start, end)
}

redisLIndex <- function(key, index) {
  .redisCmd(.raw('LINDEX'), .raw(key), index)
}

redisLSet <- function(key, index, value) {
  key <- charToRaw(as.character(key))
  index <- charToRaw(as.character(index))
  .redisCmd(.raw('LSET'), key, index, value) == 'OK'
}

redisLRem <- function(key, count, value) {
  .redisCmd(.raw('LREM'), .raw(key), .raw(as.character(count)), value)
}

redisRPopLPush <- function(src, dest) {
  .redisCmd(.raw('RPOPLPUSH'), .raw(src), .raw(dest))
}

redisBRPop <- function(keys, timeout=0) {
  keylist <- as.list(keys)
  tout <- as.character(timeout)
  x <- do.call('.redisCmd',lapply(c(list('BRPOP'),keylist,tout),charToRaw))
  if(length(x)>1) {
    n <- x[[1]]
    x <- list(x[[2]])
    names(x) <- n
  }
  x
}

redisBLPop <- function(keys, timeout=0) {
  keylist <- as.list(keys)
  tout <- as.character(timeout)
  x <- do.call('.redisCmd',lapply(c(list('BLPOP'),keylist,tout),charToRaw))
  if(length(x)>1) {
    n <- x[[1]]
    x <- list(x[[2]])
    names(x) <- n
  }
  x
}

redisBRPopLPush <- function(src, dest, timeout=0) {
  tout <- as.character(timeout)
  .redisCmd(.raw('BRPOPLPUSH'), .raw(src), .raw(dest), .raw(tout))
}
