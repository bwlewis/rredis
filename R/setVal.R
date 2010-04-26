redisSInter <- function(keys, ...)
{
  sets <- c(as.list(keys),list(...))
  do.call('.redisCmd',lapply(c(list('SINTER'),sets),charToRaw))
}

redisSUnion <- function(keys, ...)
{
  sets <- c(as.list(keys),list(...))
  do.call('.redisCmd',lapply(c(list('SUNION'),sets),charToRaw))
}

redisSUnionStore <- function(dest, keys, ...)
{
  sets <- c(as.list(dest),as.list(keys),list(...))
  do.call('.redisCmd',lapply(c(list('SUNIONSTORE'),sets),charToRaw))
}

redisSInterStore <- function(dest, keys, ...)
{
  sets <- c(as.list(dest),as.list(keys),list(...))
  do.call('.redisCmd',lapply(c(list('SINTERSTORE'),sets),charToRaw))
}

redisSDiff <- function(keys, ...)
{
  sets <- c(as.list(keys),list(...))
  do.call('.redisCmd',lapply(c(list('SDIFF'),sets),charToRaw))
}

redisSDiffStore <- function(dest, keys, ...)
{
  sets <- c(as.list(dest),as.list(keys),list(...))
  do.call('.redisCmd',lapply(c(list('SDIFFSTORE'),sets),charToRaw))
}

redisSIsMember <- function(set, element)
{
  set <- as.character(set)
  element <- as.character(element)
  1 == .redisCmd(.raw('SISMEMBER'),.raw(set),.raw(element))
}

redisSRandMember <- function(set)
{
  .redisCmd(.raw('SRANDMEMBER'),.raw(set))
}

redisSAdd <- function(set, element)
{
  .redisCmd(.raw('SADD'),.raw(set),element)
}

redisSPop <- function(set)
{
  .redisCmd(.raw('SPOP'),.raw(set))
}

redisSMembers <- function(set)
{
  .redisCmd(.raw('SMEMBERS'),.raw(set))
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
