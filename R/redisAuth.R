redisAuth <- function(pwd)
{
  msg <- paste('AUTH ',pwd,'\r\n',sep='')
  sendCmd(msg)
}
