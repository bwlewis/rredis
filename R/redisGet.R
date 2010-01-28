redisGet <- function(key) {
  msg <- paste('GET ',key,'\r\n',sep='')
  sendCmd(msg)
}
