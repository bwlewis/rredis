.redisEnv <- new.env()

.redis <-
function()
{
  tryCatch(get('con',envir=.redisEnv),error=function(e) stop('Not connected, try using redisConnect()'))
}

.redisPP <-
function()
{
# Ping-pong
  con <- .redis()
  socketSelect(list(con),write=TRUE)
  cat('PING\r\n',file=con)
  socketSelect(list(con))
  l <- readLines(con=con)
  if(substr(l,1,5)!='+PONG') stop ('Ping/pong error')
}

`.onLoad` <-
function(libname,pkgname)
{
}

# XXX add .onUnload cleanup
