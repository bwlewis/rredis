redisAuth <-
function(pwd)
{
  msg <- paste('AUTH ',pwd,'\r\n',sep='')
  con <- .redis()
  socketSelect(list(con),write=TRUE)
  cat(msg, file=con)
  socketSelect(list(con))
  l <- readLines(con,n=1)
  if(l!="+OK") stop("AUTH error\n")
# XXX reset connection
}

