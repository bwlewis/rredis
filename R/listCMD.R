redisRPush <- function(key, value) {
  value <- cerealize(value)
  msg <- paste('RPUSH ',key,' ',length(value),'\r\n',sep='')
  sendCmd(msg, value)
}

redisLPush <- function(key, value) {
  value <- cerealize(value)
  msg <- paste('LPUSH ',key,' ',length(value),'\r\n',sep='')
  sendCmd(msg, value)
}

redisLLen <- function(key) {
  msg <- paste('LLEN ',key,'\r\n',sep='')
  sendCmd(msg)
}

redisLRange <- function(key, start, end) {
  msg <- paste('LRANGE ',key,' ',start,' ',end,'\r\n',sep='')
  sendCmd(msg)
}

redisLTrim <- function(key,start,end) {
  msg <- paste('LTRIM ',key,' ',start,' ',end,'\r\n',sep='')
  sendCmd(msg)
}

redisLIndex <- function(key, index) {
  msg <- paste('LINDEX ',key,' ',start,'\r\n',sep='')
  sendCmd(msg)
}

redisLSet <- function(key, index, value) {
  msg <- paste('LSET ',key,' ',index,' ',value,'\r\n',sep='')
  sendCmd(msg)
}

redisLRem <- function(key, count, value) {
  msg <- paste('LREM ',key,' ',count,' ',value,'\r\n',sep='')
  sendCmd(msg)
}

redisRPop <- function(key) {
  msg <- paste('RPOP ',key,'\r\n',sep='')
  sendCmd(msg)
}
