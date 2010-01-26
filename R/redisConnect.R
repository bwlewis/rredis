redisConnect <-
function(host='localhost', port=6379)
{
  con <- socketConnection(host, port,open='a+b')
  assign('con',con,envir=.redisEnv)
  socketSelect(list(con),write=TRUE)
  cat('PING\r\n',file=con)
  socketSelect(list(con))
  l <- readLines(con=con)
# XXX check error
  invisible()
}

