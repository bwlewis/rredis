redisSInter <- function(keys, ...)
{
  sets <- c(as.list(keys),list(...))
  cmd <- list(charToRaw('SINTER'))
  cmd <- c(cmd, lapply(sets,charToRaw))
  .sendCmdMulti(cmd)
}

redisSUnion <- function(keys, ...)
{
  sets <- c(as.list(keys),list(...))
  cmd <- list(charToRaw('SUNION'))
  cmd <- c(cmd, lapply(sets,charToRaw))
  .sendCmdMulti(cmd)
}

redisSUnionStore <- function(dest, keys, ...)
{
  sets <- c(as.list(dest),as.list(keys),list(...))
  cmd <- list(charToRaw('SUNIONSTORE'))
  cmd <- c(cmd, lapply(sets,charToRaw))
  .sendCmdMulti(cmd)
}

redisSInterStore <- function(dest, keys, ...)
{
  sets <- c(as.list(dest),as.list(keys),list(...))
  cmd <- list(charToRaw('SINTERSTORE'))
  cmd <- c(cmd, lapply(sets,charToRaw))
  .sendCmdMulti(cmd)
}

redisSDiff <- function(keys, ...)
{
  sets <- c(as.list(keys),list(...))
  cmd <- list(charToRaw('SDIFF'))
  cmd <- c(cmd, lapply(sets,charToRaw))
  .sendCmdMulti(cmd)
}

redisSDiffStore <- function(dest, keys, ...)
{
  sets <- c(as.list(dest),as.list(keys),list(...))
  cmd <- list(charToRaw('SDIFFSTORE'))
  cmd <- c(cmd, lapply(sets,charToRaw))
  .sendCmdMulti(cmd)
}

redisSIsMember <- function(set, element)
{
  cmd <- list(SISMEMBER=charToRaw(set),element)
  1 == .sendCmdMulti(cmd)
}

redisSRandMember <- function(set)
{
  cmd <- list(SRANDMEMBER=charToRaw(set))
  .sendCmdMulti(cmd)
}

redisSAdd <- function(set, element)
{
  cmd <- list(SADD=charToRaw(set),element)
  .sendCmdMulti(cmd)
}

redisSPop <- function(set)
{
  msg <- paste('SPOP ',set,'\r\n',sep='')
  .sendCmd(msg)
}

redisSMembers <- function(set)
{
  msg <- paste('SMEMBERS ',set,'\r\n',sep='')
  .sendCmd(msg)
}

redisSRem <- function(set, element)
{
  cmd <- list(SREM=charToRaw(set),element)
  .sendCmdMulti(cmd)
}

redisSCard <- function(set)
{
  msg <- paste('SCARD ',set,'\r\n',sep='')
  .sendCmd(msg)
}

redisSMove <- function(setA, setB, element)
{
  cmd <- list(SMOVE=charToRaw(setA),charToRaw(setB),element)
  .sendCmdMulti(cmd)
}

