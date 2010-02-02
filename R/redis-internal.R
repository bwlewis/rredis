# This file contains functions and environments used internally 
# by the rredis package (not exported in the namespace).

.redisEnv <- new.env()

.redis <- function() {
  tryCatch(get('con',envir=.redisEnv),error=function(e) stop('Not connected, try using redisConnect()'))
}

.redisPP <- function() {
  # Ping-pong
  .sendCmd('PING\r\n')
}

.cerealize <- function(value) {
  if(!is.raw(value)) serialize(value,ascii=FALSE,connection=NULL)
  else value
}

.redismsg <- function(...) {
  dat <- list(...)
  paste(paste(dat,collapse=' '), '\r\n', sep='')
}

.getResponse <- function(names=NULL) {
  con <- .redis()
  socketSelect(list(con))
  l <- readLines(con=con, n=1)
  c <- substr(l, 1, 1)
  if (nchar(l) < 2) {
    if(c == '+') {
      # '+' is a valid retrun message on at least one cmd (RANDOMKEY)
      return('')
    }
    stop('Message garbled')
  }
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
           vals <- vector('list',numVars)
           if(!is.null(names)) names(vals) <- names
           for (i in 1:numVars) {
             vals[[i]] <- .getResponse()
           }
           vals
         },
         stop('Unknown message type'))
}

.sendCmd <- function(cmd, bin=NULL, checkResponse=TRUE, ...) {
  con <- .redis()
  socketSelect(list(con), write=TRUE)
  cat(cmd, file=con)
  if (!is.null(bin)) {
    socketSelect(list(con), write=TRUE)
    writeBin(bin, con)
    socketSelect(list(con), write=TRUE)
    cat('\r\n', file=con)
  }
  if (checkResponse) .getResponse(...)
}

# Requires a list of key1=value1, key2=value2, ...
# It's OK to have a NULL value in which case only the key will be transmitted.
.sendCmdMulti <- function(cmd, keyvalues) {
  numItems <- length(keyvalues)
  keys <- names(keyvalues)
  if(any(nchar(keys)==0)) stop("Invalid key name")
  foo <- paste('*', as.character((2* numItems) + 1), '\r\n',
                '$', as.character(nchar(cmd)), '\r\n',
                cmd, '\r\n', sep='')
  .sendCmd(foo,checkResponse=FALSE)
  for (i in 1:numItems) {
    keyvalues[[i]] <- .cerealize(keyvalues[[i]])
    l <- length(keyvalues[[i]])
    foo <- paste('$', as.character(nchar(keys[[i]])), '\r\n',
                keys[[i]], '\r\n', sep='')
    .sendCmd(foo, checkResponse=FALSE)
    if(l>0) {
      bar <- paste('$', as.character(l), '\r\n', sep='')
      .sendCmd(bar, bin = keyvalues[[i]], checkResponse=FALSE)
      .sendCmd('\r\n', checkResponse=FALSE)
     }
  }
  .getResponse()
}
