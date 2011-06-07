# This file contains various control functions.

# Basic response handler, only really useful in nonblocking cases
`redisGetResponse` <- function()
{
  if(!exists('count',where=.redisEnv)) return(.getResponse())
  if(.redisEnv$count < 1) return(NULL)
  replicate(.redisEnv$count, .getResponse(), simplify=FALSE)
}

`redisSetBlocking` <- function(value=TRUE)
{
  value <- as.logical(value)
  if(is.na(value)) stop("logical value required")
  assign('block',value,envir=.redisEnv)
}

`redisConnect` <-
function(host='localhost', port=6379, returnRef=FALSE)
{
  connect <- FALSE
  if(!exists("con",envir=.redisEnv)) connect <- TRUE
  else connect <-  tryCatch(!isOpen(.redisEnv$con), error=function(e) TRUE) 
  if(connect)
   {
# R Windows appears to suffer from a serious problem affecting non-blocking
# connections and readBin with raw data, see:
# http://www.mail-archive.com/r-devel@r-project.org/msg16420.html.
# We force blocking connections on Windows systems to work around this.
    if(Sys.info()[[1]] == "Windows")
      con <- socketConnection(host, port, open='a+b', blocking=TRUE)
    else
      con <- socketConnection(host, port, open='a+b')
# Stash state in the redis enivronment describing this connection:
    assign('con',con,envir=.redisEnv)
    assign('host',host,envir=.redisEnv)
    assign('port',port,envir=.redisEnv)
# Count is for nonblocking communication, it keeps track of the number of
# getResponse calls that are pending.
    assign('count',0,envir=.redisEnv)
    tryCatch(.redisPP(), 
      error=function(e) {
        cat(paste('Error: ',e,'\n'))
              close(con);
              rm(list='con',envir=.redisEnv)
            })
   }
  if(returnRef) return(.redisEnv)
  invisible()
}

`redisClose` <- 
function()
{
  con <- .redis()
  close(con)
  remove(list='con',envir=.redisEnv)
}

`redisAuth` <- 
function(pwd)
{
  .redisCmd(.raw('AUTH'), .raw(pwd))
}

`redisSave` <-
function()
{
  .redisCmd(.raw('SAVE'))
}

`redisBgSave` <-
function()
{
  .redisCmd(.raw('BGSAVE'))
}

`redisBgRewriteAOF` <-
function()
{
  .redisCmd(.raw('BGREWRITEAOF'))
}

`redisShutdown` <-
function()
{
  .redisCmd(.raw('SHUTDOWN'))
  remove(list='con',envir=.redisEnv)
}

`redisInfo` <-
function()
{
  x <- .redisCmd(.raw('INFO'))
  z <- strsplit(x,'\r\n')
  w <- unlist(lapply(z,strsplit,':'))
  n <- length(w)
  e <- seq(from=2,to=n,by=2)
  o <- seq(from=1,to=n,by=2)
  z <- as.list(w[e])
  names(z) <- w[o]
  z
}

`redisSlaveOf` <-
function(host,port)
{
# Use host="no" port="one" to disable slave replication
  .redisCmd(.raw('SLAVEOF'),.raw(as.character(host)), .raw(as.character(port)))
}

redisFlushDB <- function() {
  .redisCmd(.raw('FLUSHDB'))
}

redisFlushAll <- function() {
  .redisCmd(.raw('FLUSHALL'))
}

redisSelect <- function(index) {
  .redisCmd(.raw('SELECT'),.raw(as.character(index)))
}

redisDBSize <- function() {
  .redisCmd(.raw('DBSIZE'))
}

redisGetContext <- function() {
  .redisEnv
}

redisSetContext <- function(e=new.env())
{
  p <- environment(redisSetContext)
  unlockBinding('.redisEnv',p)
  assign('.redisEnv',e,p)
  lockBinding('.redisEnv',p)
  invisible()
}
