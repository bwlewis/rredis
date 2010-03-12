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
  s <- substr(l, 1, 1)
  if (nchar(l) < 2) {
    if(s == '+') {
      # '+' is a valid retrun message on at least one cmd (RANDOMKEY)
      return('')
    }
    stop('Message garbled')
  }
  switch(s,
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
           vals <- NULL
           numVars <- as.numeric(substr(l,2,nchar(l)))
           if(numVars > 0) {
             vals <- vector('list',numVars)
             if(!is.null(names)) names(vals) <- names
             for (i in 1:numVars) {
# XXX This extra copy is unfortunate, but the default R behavior is
# not acceptable (assigning a list entry to NULL removes it from the list!)
# Does anyone have a better idea here?
               vi <- .getResponse()
               if(!is.null(vi)) vals[[i]] <- vi
             }
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
# This represents the multi-bulk send protocol. Keys are sent as plain
# text (not as R objects), values as serialized objects.
# NA or zero-length keys are allowed, for example: 
# list(SADD=charToRaw("mykey"), myvalue)
# NA or zero-length keys are simply skipped in the outgoing message.
.sendCmdMulti <- function(keyvalues) {
  numItems <- length(keyvalues)
  keys <- names(keyvalues)
  if(is.null(keys)) {
    names(keyvalues) <- NA
    keys <- names(keyvalues)
  }
  n <- numItems + length(keys[(nchar(keys)!=0) & !is.na(keys)])
  foo <- paste('*', as.character(n), '\r\n',sep='')
  .sendCmd(foo,checkResponse=FALSE)
  for (i in 1:numItems) {
    if((nchar(keys[[i]])>0) & (!is.na(keys[[i]]))) {
      foo <- paste('$', as.character(nchar(keys[[i]])), '\r\n',
                keys[[i]], '\r\n', sep='')
      .sendCmd(foo, checkResponse=FALSE)
    }
    keyvalues[[i]] <- .cerealize(keyvalues[[i]])
    l <- length(keyvalues[[i]])
    if(l>0) {
      bar <- paste('$', as.character(l), '\r\n', sep='')
      .sendCmd(bar, bin = keyvalues[[i]], checkResponse=FALSE)
      .sendCmd('\r\n', checkResponse=FALSE)
     }
  }
  .getResponse()
}
