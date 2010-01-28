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
  socketSelect(list(con))
  l <- readLines(con=con, n=1)
  c <- substr(l, 1, 1)
  if (c == '+') {
    return(substr(l,2,nchar(l)))
  }
  if (c == ':') {
    return(as.numeric(substr(l,2,nchar(l))))
  }
  if (c == "$") {
    dat <- as.numeric(substr(l,2,nchar(l)))
    if (dat < 0) {
      return(NULL)
    }
    else {
      socketSelect(list(con))
      dat <- readBin(con, 'raw', n=dat)
      l <- readLines(con,n=1)
      # Try retrieving an R object, otherwise default to character:
      return(tryCatch(unserialize(dat),
                      error=function(e) rawToChar(dat)))
    }
  }
  else {
    cat('This is too complicated\n')
  }
}

sendCmd <- function(cmd, bin=NULL) {
  con <- .redis()
  socketSelect(list(con), write=TRUE)
  cat(cmd, file=con)
  if (!is.null(bin)) {
    socketSelect(list(con), write=TRUE)
    writeBin(bin, con)
    socketSelect(list(con), write=TRUE)
    cat('\r\n', file=con)
  }
  getResponse()
}
