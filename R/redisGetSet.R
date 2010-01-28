redisGetSet <- function(key, value) {
  if(!is.raw(value)) value <- serialize(value,ascii=FALSE,connection=NULL)
  msg <- paste('GETSET ',key,' ',length(value),'\r\n',sep='')
  sendCmd(msg,value)
}
