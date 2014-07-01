# This file contains various control functions.

# Basic response handler, only really useful in pipelined cases
# all function argument is left in for backward compatibility,
# it is not used.
`redisGetResponse` <- function(all=TRUE)
{
  if(!exists('count',where=.redisEnv$current)) return(.getResponse())
  if(.redisEnv$current$count < 1) return(NULL)
  replicate(.redisEnv$current$count, .getResponse(), simplify=FALSE)
}

# Maintained for compatability.
`redisSetBlocking` <- function(value=TRUE)
{
  warning("redisSetBlocking is deprecated. Use redisSetPipeline instead.")
  redisSetPipeline(!value)
}
`redisSetPipeline` <- function(value=FALSE)
{
  value <- as.logical(value)
  if(is.na(value)) stop("logical value required")
  assign('pipeline',value,envir=.redisEnv$current)
}

`redisConnect` <-
function(host='localhost', port=6379, password=NULL,
         returnRef=FALSE, nodelay=FALSE, timeout=2678399L)
{
  .redisEnv$current <- new.env()
  assign('pipeline',FALSE,envir=.redisEnv$current)
  con <- .openConnection(host=host, port=port, nodelay=nodelay, timeout=timeout, envir=.redisEnv$current)
  if (!is.null(password)) tryCatch(redisAuth(password), 
    error=function(e) {
      cat(paste('Error: ',e,'\n'))
            .closeConnection(con);
            rm(list='con',envir=.redisEnv$current)
          })
  tryCatch(.redisPP(), 
    error=function(e) {
      cat(paste('Error: ',e,'\n'))
            .closeConnection(con);
            rm(list='con',envir=.redisEnv$current)
          })
  if(returnRef) return(.redisEnv$current)
  invisible()
}

`redisClose` <- 
function(e)
{
  if(missing(e)) e = .redisEnv$current
  con <- .redis(e)
  .closeConnection(con)
  remove(list='con',envir=e)
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
  remove(list='con',envir=.redisEnv$current)
}

`redisInfo` <-
function()
{
  x <- .redisCmd(.raw('INFO'))
  z <- strsplit(x,'\r\n')[[1]]
  rj <- c(grep("^$",z), grep("^#",z))
  if(length(rj)>0) z <- z[-rj]
  z <- gsub(":$",": ",z,perl=TRUE)
  z <- z[grep(":",z)]
  if(length(z)<1) return(NULL)
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
  .redisEnv$current
}

redisSetContext <- function(e=NULL)
{
  if(is.null(e)) .redisEnv$current <- .redisEnv
  else {
    if(!is.environment(e)) stop("Invalid context")
    .redisEnv$current <- e
  }
}
