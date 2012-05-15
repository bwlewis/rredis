# This file contains functions and environments used internally 
# by the rredis package (not exported in the namespace).

.redisEnv <- new.env()
.redisEnv$current <- .redisEnv

.redis <- function() 
{
  if(!exists('con',envir=.redisEnv$current))
    stop('Not connected, try using redisConnect()')
  .redisEnv$current$con
}

# .redisError may be called by any function when a serious error occurs.
# It will print an indicated error message, attempt to reset the current
# Redis server connection, and signal the error.
.redisError <- function(msg)
{
  env <- .redisEnv$current
  con <- .redis()
  close(con)
# May stop with an error here on connect fail
  con <- socketConnection(env$host, env$port,open='a+b')
  assign('con',con,envir=env)
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

# Burn data in the RX buffer, used after interrupt conditions
.burn <- function()
{
  con <- .redis()
  while(socketSelect(list(con),timeout=1L))
    readBin(con, raw(), 1000000L)
  .redisError("Interrupted communincation with Redis")
}

.getResponse <- function(raw=FALSE)
{
  env <- .redisEnv$current
tryCatch({
  con <- .redis()
  socketSelect(list(con))
  l <- readLines(con=con, n=1)
  tryCatch(
    env$count <- max(env$count - 1,0),
    error = function(e) assign('count', 0, envir=env)
  )
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
               if(raw)
                 return(dat)
               else
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
             if(raw)
               do.call(c,r)
             else
               tryCatch(unserialize(do.call(c,r)),
                      error=function(e) rawToChar(do.call(c,r)))
           },
         '*' = {
           numVars <- as.integer(substr(l,2,nchar(l)))
           if(numVars > 0L) {
             replicate(numVars, .getResponse(raw=raw), simplify=FALSE)
           } else NULL
         },
         stop('Unknown message type'))
}, interrupt=function(e) .burn()
)
}

#
# .raw is just a shorthand wrapper for charToRaw:
#
.raw <- function(word) 
{
  tryCatch(charToRaw(word),warning=function(w) stop(w), error=function(e) stop(e))
}

# Expose the basic Redis interface to the user
redisCmd <- function(CMD, ..., raw=FALSE)
{
  a <- c(alist(),list(.raw(CMD)),
         lapply(list(...), function(x) 
           if(is.character(x)) charToRaw(x)
           else(.cerealize(x))
       ))
  if(raw) return(do.call('.redisRawCmd',a))
  do.call('.redisCmd', a)
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
# copy (which is limited to 2GB due to R indexing).
.redisCmd <- function(...)
{
  env <- .redisEnv$current
  con <- .redis()
  f <- match.call()
  n <- length(f) - 1
  hdr <- paste('*', as.character(n), '\r\n',sep='')
  socketSelect(list(con), write=TRUE)
#  cat(hdr, file=con)
  writeBin(.raw(hdr), con)
tryCatch({
  for(j in seq_len(n)) {
    v <- eval(f[[j+1]],envir=sys.frame(-1))
    if(!is.raw(v)) v <- .cerealize(v)
    l <- length(v)
    hdr <- paste('$', as.character(l), '\r\n', sep='')
    socketSelect(list(con), write=TRUE)
#    cat(hdr, file=con)
    writeBin(.raw(hdr), con)
    socketSelect(list(con), write=TRUE)
    writeBin(v, con)
    socketSelect(list(con), write=TRUE)
#    cat('\r\n', file=con)
    writeBin(.raw('\r\n'), con)
  }
},
error=function(e) {.redisError("Invalid agrument");invisible()},
interrupt=function(e) .burn()
)

  block <- TRUE
  if(exists('block',envir=env)) block <- get('block',envir=env)
  if(block)
    return(.getResponse())
  tryCatch(
    env$count <- env$count + 1,
    error = function(e) assign('count', 1, envir=env)
  )
  invisible()
}

.redisRawCmd <- function(...)
{
  con <- .redis()
  f <- match.call()
  n <- length(f) - 1
  hdr <- paste('*', as.character(n), '\r\n',sep='')
  socketSelect(list(con), write=TRUE)
  cat(hdr, file=con)
tryCatch({
  for(j in seq_len(n)) {
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
},
error=function(e) {.redisError("Invalid agrument");invisible()},
interrupt=function(e) .burn()
)
  .getResponse(raw=TRUE)
}
