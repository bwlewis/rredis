# This file contains functions and environments used internally 
# by the rredis package (not exported in the namespace).

.redisEnv <- new.env()
.redisEnv$current <- .redisEnv

.redis <- function(e)
{
  if(missing(e)) e = .redisEnv$current
  if(!exists('con',envir=e))
    stop('Not connected, try using redisConnect()')
  e$con
}

.openConnection <- function(host, port, nodelay=FALSE, timeout=2678399L, envir=rredis:::.redisEnv$current)
{
  stopifnot(is.character(host))
  stopifnot(is.numeric(port))
  stopifnot(is.logical(nodelay))
# We track the file descriptor of the new connection in a sneaky way
  fds <- rownames(showConnections(all=TRUE))
  con <- socketConnection(host, port, open="a+b",
                          blocking=TRUE, timeout=timeout)
  fd <- rownames(showConnections(all=TRUE))
  fd <- as.integer(setdiff(fd,fds))
  if(nodelay)
  {
    Nagle <- .Call("SOCK_NAGLE",fd,1L,PACKAGE="rredis")
    if(Nagle!=1) warning("Unable to set nodelay.")
  }
# Stash state in the redis enivronment describing this connection:
  assign('fd',fd,envir=envir)
  assign('con',con,envir=envir)
  assign('host',host,envir=envir)
  assign('port',port,envir=envir)
  assign('nodelay',nodelay,envir=envir)
# Count is for pipelined communication, it keeps track of the number of
# getResponse calls that are pending.
  assign('count',0,envir=envir)
  con
}

.closeConnection <- function(s)
{
  close(s)
}

# .redisError may be called by any function when a serious error occurs.
# It will print an indicated error message, attempt to reset the current
# Redis server connection, and signal the error.
.redisError <- function(msg, e=NULL)
{
  env <- .redisEnv$current
  con <- .redis()
  .closeConnection(con)
# May stop with an error here on connect fail
  con <- .openConnection(host=env$host,
                         port=env$port, nodelay=env$nodelay, envir=env)
  if(!is.null(e)) print(as.character(e))
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
.burn <- function(e)
{
  con <- .redis()
  while(socketSelect(list(con),timeout=1L))
    readBin(con, raw(), 1000000L)
  .redisError("Interrupted communincation with Redis",e)
}

#
# .raw is just a shorthand wrapper for charToRaw:
#
.raw <- function(word) 
{
  tryCatch(charToRaw(word),
           warning=function(w) stop(w),
           error=function(e) stop(e))
}

# Expose the basic Redis interface to the user
redisCmd <- function(CMD, ..., raw=FALSE)
{
  a <- c(alist(),list(.raw(CMD)),
         lapply(list(...), function(x) 
           if(is.character(x)) charToRaw(x)
           else(.cerealize(x))
       ))
  if(raw) a <- c(a,raw=TRUE)
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
# Check to see if a rename list exists and use it if it does...we also
# define a little helper function to handle replacing the command.
# The rename list must have the form:
# list(OLDCOMMAND="NEWCOMMAND", SOME_OTHER_CMD="SOME_OTHER_NEW_CMD",...)
  rep <- c()
  if(exists("rename",envir=.redisEnv)) rep = get("rename",envir=.redisEnv)
  f <- match.call()
# Check for raw option (which means don't deserialize returned resuts)
  raw <- FALSE
  if(any("raw" %in% names(f)))
  {
    wr  <- which(names(f)=="raw")
    raw <- f[[wr]]
    f   <- f[-wr]
  }
  n <- length(f) - 1
  hdr <- paste('*', as.character(n), '\r\n',sep='')
  writeBin(.raw(hdr), con)
  tryCatch({
    for(j in seq_len(n)) {
      if(j==1)
        v <- .renameCommand(eval(f[[j+1]],envir=sys.frame(-1)), rep)
      else
        v <- eval(f[[j+1]],envir=sys.frame(-1))
      if(!is.raw(v)) v <- .cerealize(v)
      l <- length(v)
      hdr <- paste('$', as.character(l), '\r\n', sep='')
      writeBin(.raw(hdr), con)
      writeBin(v, con)
      writeBin(.raw('\r\n'), con)
    }
  },
    error=function(e) {.redisError("Invalid agrument");invisible()},
    interrupt=function(e) .burn(e)
  )

  pipeline <- FALSE
  if(exists('pipeline',envir=env)) pipeline <- get('pipeline',envir=env)
  if(!pipeline)
    return(.getResponse(raw=raw))
  tryCatch(
    env$count <- env$count + 1,
    error = function(e) assign('count', 1, envir=env)
  )
  invisible()
}

.renameCommand <- function(x, rep)
{
  if(is.null(rep)) return(x)
  v <- rawToChar(x)
  if(v %in% names(rep)) return(charToRaw(rep[[v]]))
  x
}

.getResponse <- function(raw=FALSE)
{
  env <- .redisEnv$current
  tryCatch({
    con <- .redis()
    l <- readLines(con=con, n=1)

    if(length(l)==0) .burn("Empty")
    tryCatch(
      env$count <- max(env$count - 1,0),
      error = function(e) assign('count', 0, envir=env)
    )
    s <- substr(l, 1, 1)
    if (nchar(l) < 2) {
      if(s == "+") {
        # '+' is a valid retrun message for at least one cmd (RANDOMKEY)
        return("")
      }
      .burn("Invalid")
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
             dat <- tryCatch(readBin(con, 'raw', n=n),
                             error=function(e) .redisError(e$message))
             m <- length(dat)
             if(m==n)
             {
               l <- readLines(con,n=1)
               if(raw)
                 return(dat)
               else
                 return(tryCatch(unserialize(dat),
                         error=function(e) rawToChar(dat)))
             }
# The message was not fully recieved in one pass for whatever reason.
# We allocate a list to hold incremental messages and then concatenate it.
# This perfromance enhancement was adapted from the Rbig server package, 
# written by Steve Weston and Pat Shields.
             rlen <- 50
             j <- 1
             r <- vector('list',rlen)
             r[j] <- list(dat)
             while(m<n)
             {
               dat <- tryCatch(readBin(con, 'raw', n=(n-m)),
                            error=function (e) .redisError(e$message))
               j <- j + 1
               if(j>rlen)
               {
                 rlen <- 2*rlen
                 length(r) <- rlen
               }
               r[j] <- list(dat)
               m <- m + length(dat)
             }
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
       stop('Unknown message type')
    )
    }, interrupt=function(e) .burn(e)
  )
}
