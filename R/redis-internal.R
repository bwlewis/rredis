# This file contains functions and environments used internally 
# by the rredis package (not exported in the namespace).

.redisEnv <- new.env()

.redis <- function() 
{
  tryCatch(get('con',envir=.redisEnv),error=function(e) stop('Not connected, try using redisConnect()'))
}

# .redisError may be called by any function when a serious error occurs.
# It will print an indicated error message, attempt to reset the current
# Redis server connection, and signal the error.
.redisError <- function(msg)
{
  con <- .redis()
  close(con)
  con <- socketConnection(.redisEnv$host, .redisEnv$port,open='a+b')
  assign('con',con,envir=.redisEnv)
  stop(msg)
}

.redisPP <- function() 
{
  # Ping-pong
  .sendCmd('PING\r\n')
}

.cerealize <- function(value) 
{
  if(!is.raw(value)) serialize(value,ascii=FALSE,connection=NULL)
  else value
}

.redismsg <- function(...) 
{
  dat <- list(...)
  paste(paste(dat,collapse=' '), '\r\n', sep='')
}

.getResponse <- function(names=NULL) 
{
  con <- .redis()
  socketSelect(list(con))
  l <- readLines(con=con, n=1)
  s <- substr(l, 1, 1)
  if (nchar(l) < 2) {
    if(s == '+') {
      # '+' is a valid retrun message on at least one cmd (RANDOMKEY)
      return('')
    }
    .redisError('Message garbled')
  }
  switch(s,
         '-' = stop(substr(l,2,nchar(l))),
         '+' = substr(l,2,nchar(l)),
         ':' = as.numeric(substr(l,2,nchar(l))),
         '$' = {
             n <- as.numeric(substr(l,2,nchar(l)))
             if (n < 0) {
               return(NULL)
             }
             socketSelect(list(con))
             dat <- tryCatch(readBin(con, 'raw', n=n),
                             error=function(e) .redisError(e$message))
             m <- length(dat)
             if(m>=n) {
               l <- readLines(con,n=1)  # Trailing \r\n
               return(tryCatch(unserialize(dat),
                         error=function(e) rawToChar(dat)))
             }
# The message was not fully recieved in one pass.
# We allocate a list to hold incremental messages and then concatenate it.
# This perfromance enhancement was adapted from the Rbig server package, 
# written by Steve Weston and Pat Shields.
             rlen <- 50
             j <- 1
             r <- vector('list',rlen)
             r[j] <- list(dat)
             while(m<n) {
# Short read; we need to retrieve the rest of this message.
               socketSelect(list(con))
               dat <- tryCatch(readBin(con, 'raw', n=(n-m)),
                            error=function (e) .redisError(e$message))
               j <- j + 1
               if(j>rlen) {
                 rlen <- 2*rlen
                 length(r) <- rlen
               }
               r[j] <- list(dat)
               m <- m + length(dat)
             }
             l <- readLines(con,n=1)  # Trailing \r\n
             length(r) <- j
             # Try retrieving an R object, otherwise default to character:
             tryCatch(unserialize(do.call(c,r)),
                      error=function(e) rawToChar(do.call(c,r)))
           },
         '*' = {
           vals <- NULL
           numVars <- as.numeric(substr(l,2,nchar(l)))
           if(numVars > 0) {
             vals <- vector('list',numVars)
             if(!is.null(names)) names(vals) <- names
             for (i in 1:numVars) {
# XXX This extra copy is unfortunate, but so is the default R behavior:
# assigning a list entry to NULL removes it from the list!
# Does anyone have a better idea here?
               vi <- .getResponse()
               if(!is.null(vi)) vals[[i]] <- vi
             }
           }
           vals
         },
         stop('Unknown message type'))
}

.sendCmd <- function(cmd, bin=NULL, checkResponse=TRUE, ...) 
{
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
.sendCmdMulti <- function(keyvalues, ...) 
{
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
  .getResponse(...)
}
