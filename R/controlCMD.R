# This file contains various control functions.

`redisConnect` <-
function(host='localhost', port=6379, returnRef=FALSE)
{
  con <- socketConnection(host, port,open='a+b')
# Stash state in the redis enivronment describing this connection:
  assign('con',con,envir=.redisEnv)
  assign('host',host,envir=.redisEnv)
  assign('port',port,envir=.redisEnv)
  tryCatch(.redisPP(), 
    error=function(e) {
      cat(paste('Error: ',e,'\n'))
            close(con);
            rm(list='con',envir=.redisEnv)
          })
  if(returnRef) return(list(con=con,host=host,port=port))
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
  .sendCmd(.redismsg('AUTH',pwd))
}

`redisSave` <-
function()
{
  .sendCmd(.redismsg('SAVE'))
}

`redisBgSave` <-
function()
{
  .sendCmd(.redismsg('BGSAVE'))
}

`redisBgRewriteAOF` <-
function()
{
  .sendCmd(.redismsg('BGREWRITEAOF'))
}

`redisShutdown` <-
function()
{
  .sendCmd(.redismsg('SHUTDOWN'))
  remove(list='con',envir=.redisEnv)
}

`redisInfo` <-
function()
{
  x <- .sendCmd(.redismsg('INFO'))
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
  .sendCmd(.redismsg('SLAVEOF'),host,port)
}

redisFlushDB <- function() {
  .sendCmd(.redismsg('FLUSHDB'))
}

redisFlushAll <- function() {
  .sendCmd(.redismsg('FLUSHALL'))
}

redisSelect <- function(index) {
  .sendCmd(.redismsg('SELECT',index))
}

redisDBSize <- function() {
  .sendCmd(.redismsg('DBSIZE'))
}

