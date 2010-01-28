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
  sendCmd('PING\r\n')
}

.inlineRecv <-
function()
{
  con <- .redis()
  socketSelect(list(con))
  l <- readLines(con=con,n=1)
  if(nchar(l)<2) stop ("Truncated message")
  switch(substr(l,1,1),
    '-' = stop(l),
    '+' = substr(l,2,nchar(l)),
    ':' = substr(l,2,nchar(l)),
    '$' = substr(l,2,nchar(l)),
    '*' = substr(l,2,nchar(l)))
}

`.onLoad` <-
function(libname,pkgname)
{
}

# XXX add .onUnload cleanup
getResponse <- function() {
  con <- .redis()
  l <- readLines(con=con)
  c <- substr(l, 1, 1)
  if (c == '+') {
    return(substr(l,2,nchar(l)))
  }
  if (c == ':') {
    return(as.numeric(substr(l,2,nchar(l))))
  }
  else {
    cat('This is too complicated\n')
  }
}

sendCmd <- function(cmd) {
  con <- .redis()
  socketSelect(list(con), write=TRUE)
  cat(cmd, file=con)
  getResponse()
}
