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
  if (nchar(l) < 2) stop('Message garbled')
  c <- substr(l, 1, 1)
  switch(c,
         '-' = stop(substr(l,2,nchar(l))),
         '+' = substr(l,2,nchar(l)),
         ':' = as.numeric(substr(l,2,nchar(l))),
         '$' = {
             dat <- as.numeric(substr(l,2,nchar(l)))
             if (dat < 0) {
               return(NULL)
             }
             socketSelect(list(con))
             dat <- readBin(con, 'raw', n=dat)
             l <- readLines(con,n=1)
             # Try retrieving an R object, otherwise default to character:
             tryCatch(unserialize(dat),
                      error=function(e) rawToChar(dat))
           },
         '*' = {
           numVars <- as.numeric(substr(l,2,nchar(l)))
           vals <- list()
           for (i in 1:numVars) {
             resp <- getResponse()
             vals <- c(vals, list(resp))
           }
           vals
         },
         stop('Unknown message type'))
}

sendCmd <- function(cmd, bin=NULL, checkResponse=TRUE) {
  con <- .redis()
  socketSelect(list(con), write=TRUE)
  cat(cmd, file=con)
  if (!is.null(bin)) {
    socketSelect(list(con), write=TRUE)
    writeBin(bin, con)
    socketSelect(list(con), write=TRUE)
    cat('\r\n', file=con)
  }
  if (checkResponse) getResponse()
}

sendCmdMulti <- function(cmd, keys, values) {
  numItems <- length(keys)
  foo <- paste('*', as.character((2* numItems) + 1), '\r\n',
                '$', as.character(nchar(cmd)), '\r\n',
                cmd, '\r\n', sep='')
  sendCmd(foo,checkResponse=FALSE)
  for (i in 1:numItems) {
    foo <- paste('$', as.character(nchar(keys[[i]])), '\r\n',
                  keys[[i]], '\r\n', sep='')
    bar <- paste('$', as.character(nchar(values[[i]])), '\r\n',
                  paste(values[[i]], collapse=''), '\r\n', sep='')
    sendCmd(foo, checkResponse=FALSE)
    sendCmd(bar, checkResponse=FALSE)
  }
  getResponse()
}
