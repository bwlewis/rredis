redisExists <- function(key) {
  msg <- paste('EXISTS ',key,'\r\n',sep='')
  sendCmd(msg)==1
}

redisDelete <- function(key) {
  msg <- paste('DEL ',key,'\r\n',sep='')
  ans <- sendCmd(msg)
  if (ans == 0) warning(paste('The key',key,'was not found.'))
  ans>0
}

redisType <- function(key) {
  msg <- paste('TYPE ',key,'\r\n',sep='')
  sendCmd(msg)
}

redisKeys <- function(pattern) {
  msg <- paste('KEYS ', pattern, '\r\n', sep='')
  sendCmd(msg)
}

redisRandomKey <- function() {
  msg <- 'RANDOMKEY\r\n'
  sendCmd(msg)
}

redisRename <- function(old, new) {
  msg <- paste('RENAME ', old, ' ', new, '\r\n', sep='')
  sendCmd(msg)
}

redisRenameNX <- function(old, new) {
  msg <- paste('RENAMENX ', old, ' ', new, '\r\n', sep='')
  1==sendCmd(msg)
}

redisDBSize <- function() {
  msg <- 'DBSIZE\r\n'
  sendCmd(msg)
}

redisExpire <- function(key, seconds) {
  msg <- paste('EXPIRE ', key, ' ', seconds, '\r\n', sep='')
  1==sendCmd(msg)
}

redisExpireAt <- function(key, time) {
  msg <- paste('EXPIREAT ', key, ' ', time, '\r\n', sep='')
  1==sendCmd(msg)
}

redisTTL <- function(key) {
  msg <- paste('TTL ', key, '\r\n', sep='')
  sendCmd(msg)
}

redisSelect <- function(index) {
  msg <- paste('SELECT ', index, '\r\n', sep='')
  sendCmd(msg)
}

redisMove <- function(key, dbindex) {
  msg <- paste('MOVE ', key, ' ', dbindex, '\r\n', sep='')
  1==sendCmd(msg)
}

redisFlushDB <- function() {
  msg <- 'FLUSHDB\r\n'
  sendCmd(msg)
}

redisFlushAll <- function() {
  msg <- 'FLUSHALL\r\n'
  sendCmd(msg)
}
