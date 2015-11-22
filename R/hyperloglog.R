# Redis Hyperloglog functions

redisPfadd <- function(key, elements)
{
  do.call(.redisCmd, lapply(c(list('PFADD'), as.list(key), as.list(elements)), charToRaw))
}

redisPfcount <- function(keys)
{
  do.call(.redisCmd, lapply(c(list('PFCOUNT'), as.list(keys)), charToRaw))
}

redisPfmerge <- function(destkey, sourcekeys)
{
  do.call(.redisCmd, lapply(c(list('PFMERGE'), as.list(destkey), as.list(sourcekeys)), charToRaw))
}
