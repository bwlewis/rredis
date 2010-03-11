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

