redisDelete <-
function(key)
{
  msg <- paste('DEL ',key,'\r\n',sep='')
  con <- .redis()
  socketSelect(list(con),write=TRUE)
  cat(msg, file=con)
  l <- .inlineRecv()
  l <- as.numeric(l)
  if(l<0) {
# Unexpected result, reset connection? XXX
    stop('DEL error occured')
  }
  if(l==0) warning(paste('The key',key,'was not found.'))
  invisible()
}
