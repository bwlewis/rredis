redisGet <-
function(key)
{
  msg <- paste('GET ',key,'\r\n',sep='')
  con <- .redis()
  socketSelect(list(con),write=TRUE)
  cat(msg, file=con)
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

