redisGet <- function(key) {
  msg <- paste('GET ',key,'\r\n',sep='')
  sendCmd(msg)
}

redisSet <- function(key, value, NX=FALSE) {
  if (!is.raw(value)) value <- serialize(value,ascii=FALSE,connection=NULL)
  if (NX) cmd <- 'SETNX ' else cmd <- 'SET '
  msg <- paste(cmd,key,' ',length(value),'\r\n',sep='')
  ret <- sendCmd(msg,value)
  if (NX) 1==ret else ret
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

# Is this the right API for this? Maybe list with key=value? -PS
# Also, lapply "optimization" might be a red herring here.  This
# might need to be changed to a for loop. -PS
redisMSet <- function(keys, values, NX=FALSE) {
  if (NX) cmd <- 'MSETNX' else cmd <- 'MSET'
  cereal <- function(value) {
    if(!is.raw(value)) serialize(value,ascii=FALSE,connection=NULL)
  }
  values <- lapply(values, cereal)
  ret <- sendCmdMulti(cmd, keys, values)
  if (NX) 1==ret else ret
}
