# A utility function that strips a named argument 'raw' from the list.
# This function returns a list with two items:
# 1. The stripped list without the raw=whatever entry
# 2. The value of the named raw=whatever entry
#
# Use this with functions that have variable length arument lists.
.redisStripRawArg <- function(List)
{
  ir <- which(names(List) %in% "raw")
  raw <- c()
  if(length(ir)>0)
  {
    raw <- List[[ir]]
    List <- List[-ir]
  }
  list(List=List, raw=raw)
}

redisSInter <- function(keys, ...)
{
  L <- .redisStripRawArg(list(...))
  sets <- c(as.list(keys),L$List)
  do.call('.redisCmd',c(lapply(c(list('SINTER'),sets),charToRaw),raw=L$raw))
}

redisSUnion <- function(keys, ...)
{
  L <- .redisStripRawArg(list(...))
  sets <- c(as.list(keys),L$List)
  do.call('.redisCmd',c(lapply(c(list('SUNION'),sets),charToRaw),raw=L$raw))
}

redisSUnionStore <- function(dest, keys, ...)
{
  L <- .redisStripRawArg(list(...))
  sets <- c(as.list(dest),as.list(keys),L$List)
  do.call('.redisCmd',c(lapply(c(list('SUNIONSTORE'),sets),charToRaw)
                        ,raw=L$raw))
}

redisSInterStore <- function(dest, keys, ...)
{
  L <- .redisStripRawArg(list(...))
  sets <- c(as.list(dest),as.list(keys),L$List)
  do.call('.redisCmd',c(lapply(c(list('SINTERSTORE'),sets),charToRaw),
                        ,raw=L$raw))
}

redisSDiff <- function(keys, ...)
{
  L <- .redisStripRawArg(list(...))
  sets <- c(as.list(keys),L$List)
  do.call('.redisCmd',c(lapply(c(list('SDIFF'),sets),charToRaw),
                        ,raw=L$raw))
}

redisSDiffStore <- function(dest, keys, ...)
{
  L <- .redisStripRawArg(list(...))
  sets <- c(as.list(dest),as.list(keys),L$List)
  do.call('.redisCmd',c(lapply(c(list('SDIFFSTORE'),sets),charToRaw),
                        ,raw=L$raw))
}

redisSIsMember <- function(set, element)
{
  set <- as.character(set)
  element <- as.character(element)
  1 == .redisCmd(.raw('SISMEMBER'),.raw(set),.raw(element))
}

redisSRandMember <- function(set,...)
{
  .redisCmd(.raw('SRANDMEMBER'),.raw(set),...)
}

redisSAdd <- function(set, element)
{
  .redisCmd(.raw('SADD'),.raw(set),element)
}

redisSPop <- function(set,...)
{
  .redisCmd(.raw('SPOP'),.raw(set),...)
}

redisSMembers <- function(set,...)
{
  .redisCmd(.raw('SMEMBERS'),.raw(set),...)
}

redisSRem <- function(set, element)
{
  .redisCmd(.raw('SREM'),.raw(set),element)
}

redisSCard <- function(set)
{
  .redisCmd(.raw('SCARD'),.raw(set))
}

redisSMove <- function(setA, setB, element)
{
  .redisCmd(.raw('SMOVE'),.raw(setA),.raw(setB),element)
}
