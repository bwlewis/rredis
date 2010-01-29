.redisEnv <- new.env()

.redis <- function() {
  tryCatch(get('con',envir=.redisEnv),error=function(e) stop('Not connected, try using redisConnect()'))
}

.redisPP <- function() {
  # Ping-pong
  sendCmd('PING\r\n')
}

cerealize <- function(value) {
  if(!is.raw(value)) serialize(value,ascii=FALSE,connection=NULL)
}

msg <- function(...) {
  dat <- list(...)
  paste(paste(dat,collapse=' '), '\r\n', sep='')
}

getResponse <- function() {
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

# The multi bulk command protocol.  I set this up to work with MSET.
# I expect it will need a refactor (soon) to clean up and a
# rework (eventually) to make it more general. -PS
sendCmdMulti <- function(cmd, keys, values) {
  numItems <- length(keys)
  foo <- paste('*', as.character((2* numItems) + 1), '\r\n',
                '$', as.character(nchar(cmd)), '\r\n',
                cmd, '\r\n', sep='')
  sendCmd(foo,checkResponse=FALSE)
  for (i in 1:numItems) {
    foo <- paste('$', as.character(nchar(keys[[i]])), '\r\n',
                  keys[[i]], '\r\n', sep='')
    bar <- paste('$', as.character(length(values[[i]])), '\r\n', sep='')
    sendCmd(foo, checkResponse=FALSE)
    sendCmd(bar, bin = values[[i]], checkResponse=FALSE)
    sendCmd('\r\n', checkResponse=FALSE)
  }
  getResponse()
}
