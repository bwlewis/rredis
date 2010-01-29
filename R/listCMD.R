redisRPush <- function(key, value) {
  value <- cerealize(value)
  sendCmd(msg('RPUSH',key,length(value)), value)
}

redisLPush <- function(key, value) {
  value <- cerealize(value)
  sendCmd(msg('LPUSH',key,length(value)), value)
}

redisLLen <- function(key) {
  sendCmd(msg('LLEN',key))
}

redisLRange <- function(key, start, end) {
  sendCmd(msg('LRANGE', key, start, end))
}

redisLTrim <- function(key,start,end) {
  sendCmd(msg('LTRIM', key, start, end))
}

redisLIndex <- function(key, index) {
  sendCmd(msg('LINDEX', key, start))
}

redisLSet <- function(key, index, value) {
  sendCmd(msg('LSET', key, index, value))
}

redisLRem <- function(key, count, value) {
  sendCmd(msg('LREM', key, count, value))
}

redisRPop <- function(key) {
  sendCmd(msg('RPOP', key))
}

redisRPopLPush <- function(src, dest) {
  sendCmd(msg('RPOPLPUSH',src,dest))
}

redisBRPop <- function(keys, timeout=0) {
  sendCmd(msg('BRPOP', paste(keys, collapse=' '), timeout))
}

redisBLPop <- function(keys, timeout=0) {
  sendCmd(msg('BLPOP', paste(keys, collapse=' '), timeout))
}
