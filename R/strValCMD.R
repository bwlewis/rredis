redisGet <- function(key) {
  msg <- paste('GET ',key,'\r\n',sep='')
  sendCmd(msg)
}

redisSet <- function(key, value) {
  if(!is.raw(value)) value <- serialize(value,ascii=FALSE,connection=NULL)
  msg <- paste('SET ',key,' ',length(value),'\r\n',sep='')
  sendCmd(msg,value)
}

redisGetSet <- function(key, value) {
  if(!is.raw(value)) value <- serialize(value,ascii=FALSE,connection=NULL)
  msg <- paste('GETSET ',key,' ',length(value),'\r\n',sep='')
  sendCmd(msg,value)
}

redisMGet <- function(keys) {
  msg <- paste('MGET ',paste(keys, collapse=' '),'\r\n',sep='')
  sendCmd(msg)
}
