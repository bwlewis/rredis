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

.openConnection <- function(host, port, nodelay=FALSE)
{
  stopifnot(typeof(host)=="character")
  stopifnot(class(port)=="numeric")
  stopifnot(typeof(nodelay)=="logical")
  .SOCK_CONNECT(host, port, as.integer(nodelay))
}

.closeConnection <- function(s)
{
  .SOCK_CLOSE(s)
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
  con <- .openConnection(env$host, env$port, env$nodelay)
  assign('con',con,envir=env)
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
  .SOCK_RECV(con)
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
# Check to see if a rename list exists and use it if it does...we also
# define a little helper function to handle replacing the command.
# The rename list must have the form:
# list(OLDCOMMAND="NEWCOMMAND", SOME_OTHER_CMD="SOME_OTHER_NEW_CMD",...)
  rep = c()
  if(exists("rename",envir=.redisEnv)) rep = get("rename",envir=.redisEnv)
  f <- match.call()
  n <- length(f) - 1
  hdr <- paste('*', as.character(n), '\r\n',sep='')
  .SOCK_SEND(con, .raw(hdr))
  tryCatch({
    for(j in seq_len(n)) {
      if(j==1)
        v <- .renameCommand(eval(f[[j+1]],envir=sys.frame(-1)), rep)
      else
        v <- eval(f[[j+1]],envir=sys.frame(-1))
      if(!is.raw(v)) v <- .cerealize(v)
      l <- length(v)
      hdr <- paste('$', as.character(l), '\r\n', sep='')
      .SOCK_SEND(con, .raw(hdr))
      .SOCK_SEND(con, v)
      .SOCK_SEND(con, .raw("\r\n"))
    }
  },
    error=function(e) {.redisError("Invalid agrument");invisible()},
    interrupt=function(e) .burn(e)
  )

  pipeline <- FALSE
  if(exists('pipeline',envir=env)) pipeline <- get('pipeline',envir=env)
  if(!pipeline)
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
# Check to see if a rename list exists and use it if it does...we also
  rep = c()
  if(exists("rename",envir=.redisEnv)) rep = get("rename",envir=.redisEnv)
    .SOCK_SEND(con, hdr)
  tryCatch({
    for(j in seq_len(n)) {
      if(j==1)
        v <- .renameCommand(eval(f[[j+1]],envir=sys.frame(-1)), rep)
      else
        v <- eval(f[[j+1]],envir=sys.frame(-1))
      if(!is.raw(v)) v <- .cerealize(v)
      l <- length(v)
      hdr <- paste('$', as.character(l), '\r\n', sep='')
      .SOCK_SEND(con, hdr)
      .SOCK_SEND(con, v)
      .SOCK_SEND(con, "\r\n")
    }
  },
    error=function(e) {.redisError("Invalid agrument");invisible()},
    interrupt=function(e) .burn(e)
  )
  .getResponse(raw=TRUE)
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
    l <- .SOCK_GETLINE(con)

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
             dat <- tryCatch(.SOCK_RECV_N(con, N=n),
                             error=function(e) .redisError(e$message))
             m <- length(dat)
             if(m==n) {
               l <- .SOCK_GETLINE(con)  # Trailing \r\n
               if(raw)
                 return(dat)
               else
                 return(tryCatch(unserialize(dat),
                         error=function(e) rawToChar(dat)))
             }
             .burn("Truncated response")
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
