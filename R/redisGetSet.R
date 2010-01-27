redisGetSet <-
function(key, value)
{
  con <- .redis()
  if(!is.raw(value)) value <- serialize(value,ascii=FALSE,connection=NULL)
  msg <- paste('GETSET ',key,' ',length(value),'\r\n',sep='')
  socketSelect(list(con),write=TRUE)
  cat(msg, file=con)
  socketSelect(list(con),write=TRUE)
  writeBin(value,con)
  socketSelect(list(con),write=TRUE)
  cat('\r\n', file=con)
  l <- .inlineRecv()
  l <- as.numeric(l)
  if(l<0) {
# Unexpected result, reset connection? XXX
    return(NULL)
  }
  socketSelect(list(con))
  x <- readBin(con,'raw',n=l)
  socketSelect(list(con))
  l <- readLines(con,n=1)
# Try retrieving an R object, otherwise default to character:
  tryCatch(unserialize(x),error=function(e) rawToChar(x))
}

