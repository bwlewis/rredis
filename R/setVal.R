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

# Generic set operation
.redisSetOp <- function(cmd, keys, ...)
{
  L <- .redisStripRawArg(list(...))
  sets <- c(as.list(keys), L$List)
  call <- lapply(c(list(cmd), sets), charToRaw)
  if(!is.null(L$raw)) call <- c(call, raw=L$raw)
  do.call(".redisCmd", call)
}

redisSInter <- function(keys, ...)
{
  .redisSetOp("SINTER", keys, ...)
}

redisSUnion <- function(keys, ...)
{
  .redisSetOp("SUNION", keys, ...)
}

redisSUnionStore <- function(dest, keys, ...)
{
  .redisSetOp("SUNIONSTORE", c(dest, keys), ...)
}

redisSInterStore <- function(dest, keys, ...)
{
  .redisSetOp("SINTERSTORE", c(dest, keys), ...)
}

redisSDiff <- function(keys, ...)
{
  .redisSetOp("SDIFF", keys, ...)
}

redisSDiffStore <- function(dest, keys, ...)
{
  .redisSetOp("SDIFFSTORE", c(dest, keys), ...)
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
