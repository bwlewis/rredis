# This file contains functions that operate on Redis 'hash' values.

redisHGet <- function(key, field, raw=FALSE) {
  if(raw){
    .redisRawCmd(.raw('HGET'), .raw(key), .raw(field))
  }else{
    .redisCmd(.raw('HGET'), .raw(key), .raw(field))
  }
}

redisHSet <- function(key, field, value, NX=FALSE) {
  value <- .cerealize(value)
  cmd <- 'HSET'
  if(NX) cmd <- 'HSETNX'
  1 == .redisCmd(.raw(cmd), .raw(key), .raw(field), value)
}

redisHMSet <- function(key, values) {
  a <- c(alist(),list(.raw('HMSET')))
  fieldnames <- lapply(names(values), charToRaw)
  a <- c(a, Map(list, fieldnames, values, USE.NAMES=FALSE))
  retval <- do.call('.redisCmd', a)
  'OK' == retval
}

redisHIncrBy <- function(key, field, value)
{
  .redisCmd(.raw('HINCRBY'),.raw(key),.raw(field),.raw(as.character(value)))
}

redisHExists <- function(key, field)
{
  .redisCmd(.raw('HEXISTS'), .raw(key), .raw(field)) == 1
}

redisHDel <- function(key, field)
{
  .redisCmd(.raw('HDEL'), .raw(key), .raw(field))
}

redisHLen <- function(key)
{
  .redisCmd(.raw('HLEN'), .raw(key))
}

redisHFields <- function(key)
{
  .redisCmd(.raw('HKEYS'), .raw(key))
}

redisHKeys <- function(key)
{
  redisHFields(key)
}

redisHVals <- function(key)
{
  .redisCmd(.raw('HVALS'), .raw(key))
}

redisHGetAll <- function(key)
{
  retval <- NULL
  all <- .redisCmd(.raw('HGETALL'), .raw(key))
  if(!is.null(all) && length(all)>1) {
    retval <- all[seq(2,length(all),by=2)]
    names(retval) <- all[seq(1,length(all),by=2)]
  }
  retval
}
