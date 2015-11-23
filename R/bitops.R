redisSetBit <- function(key, offset, bit)
{
  .redisCmd(.raw('SETBIT'), .raw(key), .raw(as.character(offset)), .raw(as.character(bit)))
}

redisGetBit <- function(key, offset, ...)
{
  .redisCmd(.raw('GETBIT'), .raw(key), .raw(as.character(offset)),...)
}

redisBitCount <- function(key)
{
  .redisCmd(.raw('BITCOUNT'), .raw(key))
}

redisBitOp <- function(operation, destkey, sourcekeys, ...)
{
  sets <- c(as.list(sourcekeys))
  do.call('.redisCmd',c(lapply(c(list('BITOP'),operation, destkey, sets),charToRaw),...))
}
