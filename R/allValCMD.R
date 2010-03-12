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

redisKeys <- function(pattern="*") 
{
  .sendCmd(.redismsg('KEYS',pattern))
}

redisRandomKey <- function() {
  .sendCmd(.redismsg('RANDOMKEY'))
}

redisRename <- function(old, new, NX=FALSE) {
  if (NX) cmd <- 'RENAMENX ' else cmd <- 'RENAME '
  ret <- .sendCmd(.redismsg(cmd,old,new))
  if (NX) 1==ret else ret
}

redisExpire <- function(key, seconds) {
  1==.sendCmd(.redismsg('EXPIRE',key,seconds))
}

redisExpireAt <- function(key, time) {
  1==.sendCmd(.redismsg('EXPIREAT',key,time))
}

redisTTL <- function(key) {
  .sendCmd(.redismsg('TTL',key))
}

redisMove <- function(key, dbindex) {
  1==.sendCmd(.redismsg('MOVE',key,dbindex))
}
