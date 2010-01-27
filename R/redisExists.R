redisExists <-
function(key)
{
  msg <- paste('EXISTS ',key,'\r\n',sep='')
  con <- .redis()
  socketSelect(list(con),write=TRUE)
  cat(msg, file=con)
  l <- .inlineRecv()
  l <- as.numeric(l)
  if(l<0) {
# Unexpected result, reset connection? XXX
    return(NULL)
  }
  if(l==0) return(FALSE)
  return(TRUE)
}
