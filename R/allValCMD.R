# This file contains functions that operate on all kinds of Redis values.

redisMulti <- function()
{
  .redisCmd(.raw('MULTI'))
}

redisExec <- function()
{
  .redisCmd(.raw('EXEC'))
}

redisDiscard <- function()
{
  .redisCmd(.raw('DISCARD'))
}

redisWatch <- function(keys)
{
  cmd <- 'WATCH'
  keys <- as.list(keys)
  keys <- lapply(keys, charToRaw)
  do.call('.redisCmd', c(list(.raw(cmd)),keys))
}

redisUnwatch <- function()
{
  cmd <- 'UNWATCH'
  do.call('.redisCmd', list(.raw(cmd)))
}

redisExists <- function(key) 
{
  .redisCmd(.raw('EXISTS'), .raw(key)) == '1'
}

redisDelete <- function(key) 
{
  keylist <- as.list(key)
  nkeys <- length(keylist)
  ans <- do.call('.redisCmd',lapply(c(list('DEL'),keylist),charToRaw))
  tryCatch(
  {
    if (ans == 0) warning(paste('No keys were deleted!'))
    else if(nkeys != ans) {
      w1 = ifelse(ans==1,'was','were')
      w2 = ifelse((nkeys-ans)==1,' was',' were')
      warning(paste(as.character(ans), ' keys ',w1,' deleted, but ',
                  as.character(nkeys - ans), w2, ' not!', sep=''))
    }
  }, error=function(e) NULL) # in case imbedded in redisMulti()
  ans
}

redisType <- function(key) 
{
  .redisCmd(.raw('TYPE'), .raw(key))
}

redisKeys <- function(pattern="*") 
{
  res <- .redisCmd(.raw('KEYS'), .raw(pattern))
  unlist(res)
}

redisRandomKey <- function() 
{
  .redisCmd(.raw('RANDOMKEY'))
}

redisRename <- function(old, new, NX=FALSE) 
{
  if (NX) cmd <- 'RENAMENX' else cmd <- 'RENAME'
  .redisCmd(.raw(cmd),.raw(old),.raw(new))
}

redisPexpire <- function(key, milliseconds)
{
  .redisCmd(.raw('PEXPIRE'),.raw(key),.raw(as.character(milliseconds)))
}

redisPexpireAt <- function(key, time)
{
  .redisCmd(.raw('PEXPIREAT'),.raw(key),.raw(as.character(time)))
}

redisPTTL <- function(key)
{
  .redisCmd(.raw('PTTL'),.raw(key))
}

redisPersist <- function(key)
{
  .redisCmd(.raw('PERSIST'),.raw(key))
}

redisExpire <- function(key, seconds) 
{
  .redisCmd(.raw('EXPIRE'),.raw(key),.raw(as.character(seconds)))
}

redisExpireAt <- function(key, time) 
{
  .redisCmd(.raw('EXPIREAT'),.raw(key),.raw(as.character(time)))
}

redisTTL <- function(key) 
{
  .redisCmd(.raw('TTL'),.raw(key))
}

redisMove <- function(key, dbindex) 
{
  .redisCmd(.raw('MOVE'),.raw(key),.raw(as.character(dbindex)))
}
