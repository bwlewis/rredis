redisDelete <- function(key) {
  msg <- paste('DEL ',key,'\r\n',sep='')
  ans <- sendCmd(msg)
  if (ans == 0) warning(paste('The key',key,'was not found.'))
  ans==0
}
