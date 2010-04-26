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
  .redisCmd(.raw('PING'))
}

.cerealize <- function(value) 
{
  if(!is.raw(value)) serialize(value,ascii=FALSE,connection=NULL)
  else value
}


.getResponse <- function()
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
             if(m==n) {
               socketSelect(list(con))
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
             socketSelect(list(con))
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

#
# .raw is just a shorthand wrapper for charToRaw:
#
.raw <- function(word) 
{
  charToRaw(word)
}

# .redisCmd corresponds to the Redis "multi bulk" protocol. It 
# expects an argument list of command elements. Arguments that 
# are not of type raw are serialized.
# Examples:
# .redisCmd(.raw('INFO'))
# .redisCmd(.raw('SET'),.raw('X'), runif(5))
#
# We use match.call here instead of, for example, as.list() to try to 
# avoid making unnecessary copies of (potentially large) function arguments.
#
# We can further improve this by writing a shadow serialization routine that
# quickly computes the length of a serialized object without serializing it.
# Then, we could serialize directly to the connection, avoiding the temporary
# copy (which, unfortunately, is limited to 2GB due to R indexing).
.redisCmd <- function(...)
{
  con <- .redis()
  f <- match.call()
  n <- length(f) - 1
  hdr <- paste('*', as.character(n), '\r\n',sep='')
  socketSelect(list(con), write=TRUE)
  cat(hdr, file=con)
  for(j in 1:n) {
    v <- eval(f[[j+1]],envir=sys.frame(-1))
    if(!is.raw(v)) v <- .cerealize(v)
    l <- length(v)
    hdr <- paste('$', as.character(l), '\r\n', sep='')
    socketSelect(list(con), write=TRUE)
    cat(hdr, file=con)
    socketSelect(list(con), write=TRUE)
    writeBin(v, con)
    socketSelect(list(con), write=TRUE)
    cat('\r\n', file=con)
  }
  .getResponse()
}
