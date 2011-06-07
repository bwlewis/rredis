`redisSubscribe` <- function(channels, pattern=FALSE)
{
  cmd <- 'SUBSCRIBE'
  channels <- as.list(channels)
  channels <- lapply(channels, charToRaw)
  if(pattern) {
    cmd <- 'PSUBSCRIBE'
    if(length(channels)>1)
      warning("Pattern subscription with multiple arguments")
  }
  x <- do.call('.redisCmd', c(list(.raw(cmd)),channels))
  len <- length(channels) - 1L
  if(len > 0L)
    x <- c(x, replicate(len, .getResponse(), simplify=FALSE))
  x
}

`redisUnsubscribe` <- function(channels, pattern=FALSE)
{
  cmd <- 'UNSUBSCRIBE'
  channels <- as.list(channels)
  channels <- lapply(channels, charToRaw)
  if(pattern){
    cmd <- 'PUNSUBSCRIBE'
    if(length(channels)>1) warning("Pattern subscription with multiple arguments")
  }
  x <- do.call('.redisCmd', c(list(.raw(cmd)),channels))
  len <- length(channels) - 1L
  if(len > 0L)
    x <- c(x, replicate(len, .getResponse(), simplify=FALSE))
  x
}

`redisPublish` <- function(channel, message)
{
  do.call('.redisCmd', list(.raw('PUBLISH'),.raw(channel),message))
}

# Callback handler
`redisMonitorChannels` <- function()
{
  x <- .getResponse()
  if(length(x)!=3 && x[[1]] != "message") return(x)
  if(exists(x[[2]],mode="function")) {
    return(do.call(x[[2]],as.list(x[[3]])))
  }
  x
}
