# This file contains functions that operate on all kinds of Redis values.

redisExists <- function(key) {
  msg <- paste('EXISTS ',key,'\r\n',sep='')
  .sendCmd(msg)==1
}

redisDelete <- function(key) {
  nkeys <- length(key)
  if (nkeys > 1) {
    key <- paste(key, collapse=' ')
  }
  msg <- paste('DEL ',key,'\r\n',sep='')
  ans <- .sendCmd(msg)
  if (ans == 0) warning(paste('No keys were deleted!'))
  else if(nkeys != ans)
    warning(paste(as.character(ans), ' keys were deleted, but ',
                  as.character(nkeys - ans), ' were not!', sep=''))
  ans==nkeys
}

redisType <- function(key) {
  msg <- paste('TYPE ',key,'\r\n',sep='')
  .sendCmd(msg)
}

redisKeys <- function(pattern) {
  msg <- paste('KEYS ', pattern, '\r\n', sep='')
  .sendCmd(msg)
}

redisRandomKey <- function() {
  msg <- 'RANDOMKEY\r\n'
  .sendCmd(msg)
}

redisRename <- function(old, new, NX=FALSE) {
  if (NX) cmd <- 'RENAMENX ' else cmd <- 'RENAME '
  msg <- paste(cmd, old, ' ', new, '\r\n', sep='')
  ret <- .sendCmd(msg)
  if (NX) 1==ret else ret
}

redisDBSize <- function() {
  msg <- 'DBSIZE\r\n'
  .sendCmd(msg)
}

redisExpire <- function(key, seconds) {
  msg <- paste('EXPIRE ', key, ' ', seconds, '\r\n', sep='')
  1==.sendCmd(msg)
}

redisExpireAt <- function(key, time) {
  msg <- paste('EXPIREAT ', key, ' ', time, '\r\n', sep='')
  1==.sendCmd(msg)
}

redisTTL <- function(key) {
  msg <- paste('TTL ', key, '\r\n', sep='')
  .sendCmd(msg)
}

redisSelect <- function(index) {
  msg <- paste('SELECT ', index, '\r\n', sep='')
  .sendCmd(msg)
}

redisMove <- function(key, dbindex) {
  msg <- paste('MOVE ', key, ' ', dbindex, '\r\n', sep='')
  1==.sendCmd(msg)
}

redisFlushDB <- function() {
  msg <- 'FLUSHDB\r\n'
  .sendCmd(msg)
}

redisFlushAll <- function() {
  msg <- 'FLUSHALL\r\n'
  .sendCmd(msg)
}
