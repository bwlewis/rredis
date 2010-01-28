.redisEnv <- new.env()

.redis <- function() {
  tryCatch(get('con',envir=.redisEnv),error=function(e) stop('Not connected, try using redisConnect()'))
}

.redisPP <- function() {
  # Ping-pong
  sendCmd('PING\r\n')
}

getResponse <- function() {
  con <- .redis()
  socketSelect(list(con))
  l <- readLines(con=con, n=1)
  c <- substr(l, 1, 1)
  switch(c,
         '-' = stop(substr(l,2,nchar(l))),
         '+' = substr(l,2,nchar(l)),
         ':' = as.numeric(substr(l,2,nchar(l))),
         "$" = {
           dat <- as.numeric(substr(l,2,nchar(l)))
           if (dat < 0) {
             return(NULL)
           }
           else {
             socketSelect(list(con))
             dat <- readBin(con, 'raw', n=dat)
             l <- readLines(con,n=1)
             # Try retrieving an R object, otherwise default to character:
             tryCatch(unserialize(dat),
                      error=function(e) rawToChar(dat))
           }
         }, stop('Unknown message type'))
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
