
redisSAdd <- function(set, element)
{
  cmd <- list(element)
  names(cmd) <- set
  .sendCmdMulti('SADD',cmd)
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

redisSRem <- function(set)
{
  msg <- paste('SREM ',set,'\r\n',sep='')
  .sendCmd(msg)
}

redisSCard <- function(set)
{
  msg <- paste('SCARD ',set,'\r\n',sep='')
  .sendCmd(msg)
}

redisSMove <- function(setA, setB, element)
{
  msg <- paste('SMOVE ',setA,' ',setB, ' ',element,'\r\n',sep='')
  .sendCmd(msg)
}

