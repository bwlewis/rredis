redisSet <-
function(key, value)
{
  con <- .redis()
  if(!is.raw(value)) value <- serialize(value,ascii=FALSE,connection=NULL)
  msg <- paste('SET ',key,' ',length(value),'\r\n',sep='')
  socketSelect(list(con),write=TRUE)
  cat(msg, file=con)
  socketSelect(list(con),write=TRUE)
  writeBin(value,con)
  socketSelect(list(con),write=TRUE)
  cat('\r\n', file=con)
  socketSelect(list(con))
  l <- readLines(con,n=1)
# XXX check for '+OK' reply
  socketSelect(list(con),write=TRUE)
  cat('\r\n', file=con)
}

