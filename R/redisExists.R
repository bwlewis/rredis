redisExists <-
function(key)
{
  msg <- paste('EXISTS ',key,'\r\n',sep='')
  sendCmd(msg)
}
