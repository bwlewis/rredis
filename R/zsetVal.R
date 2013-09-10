# Redis ordered set functions

redisZAdd <- function(key, score, member)
{
  .redisCmd(.raw('ZADD'),.raw(key), .raw(as.character(score)), member)
}

redisZRem <- function(key, member)
{
  .redisCmd(.raw('ZREM'),.raw(key), member)
}

redisZIncrBy <- function(key, member, increment,...)
{
  z <- NULL
  .redisCmd(.raw('ZINCRBY'),.raw(key), .raw(as.character(increment)),
                 member,...)
}

redisZRank <- function(key, member, decreasing=FALSE,...)
{
  cmd <- .raw('ZRANK')
  if(decreasing) cmd <- .raw('ZREVRANK')
  .redisCmd(cmd,.raw(key), member,...)
}

redisZRange <- function(key, start=0, end=-1, decreasing=FALSE,
                        withscores=FALSE,...)
{
  cmd <- .raw('ZRANGE')
  if(decreasing) cmd <- .raw('ZREVRANGE')
  start <- as.character(start)
  end <- as.character(end)
  if(withscores) {
    z <- .redisCmd(cmd,.raw(key),.raw(start),.raw(end),.raw('WITHSCORES'),...)
    if(!is.null(z)) {
      return(list(elements=z[seq(1,length(z),by=2)],
                  scores=as.list(as.numeric(z[seq(2,length(z),by=2)]))))
    }
   }
  else
    .redisCmd(cmd,.raw(key),.raw(start),.raw(end),...)
}

redisZRangeByScore <- function(key, min, max, offset=NULL, count=NULL, withscores=FALSE,...)
{
  min <- as.character(min)
  max <- as.character(max)
  a <- c(alist(), list(.raw('ZRANGEBYSCORE'), .raw(key), .raw(min), .raw(max)),...)
  if(!is.null(offset) && !is.null(count)) {
    a <- c(a, list(.raw('LIMIT'), .raw(offset), .raw(count)))
  }
  if(withscores) {
    a <- c(a,list(.raw('WITHSCORES')))
    z <- do.call('.redisCmd',a)
    if(!is.null(z)) {
      return(list(elements=z[seq(1,length(z),by=2)],
                  scores=as.list(as.numeric(z[seq(2,length(z),by=2)]))))
    }
   }
  do.call('.redisCmd', a)
}

redisZRemRangeByRank <- function(key, start, end)
{
  start <- as.character(start)
  end <- as.character(end)
  .redisCmd(.raw('ZREMRANGEBYRANK'), .raw(key), .raw(start), .raw(end))
}

redisZRemRangeByScore <- function(key, min, max)
{
  min <- as.character(min)
  max <- as.character(max)
  .redisCmd(.raw('ZREMRANGEBYSCORE'), .raw(key), .raw(min), .raw(max))
}

redisZCard <- function(key,...)
{
  .redisCmd(.raw('ZCARD'), .raw(key))
}

redisZScore <- function(key, element)
{
  ret <- .redisCmd(.raw('ZSCORE'), .raw(key), .raw(element))
  if(!is.null(ret)) ret <- as.numeric(ret)
  ret
}

.zinu <- function(type, dstkey, keys, weights=c(), aggregate=NULL,...)
{
  N <- length(keys)
  a <- c(alist(), list(.raw(type), .raw(dstkey), .raw(as.character(N))),...)
  sets <- lapply(keys,charToRaw)
  a <- c(a, sets)
  if(!is.null(weights)) {
    a <- c(a, list(.raw('WEIGHTS')), lapply(as.character(weights), charToRaw))
  }
  if(!is.null(aggregate)) {
    a <- c(a, list(.raw('AGGREGATE'), .raw(aggregate)))
  }
  do.call('.redisCmd', a)
}

redisZInterStore <- function(dstkey, keys, weights=c(), aggregate=NULL,...)
{
  .zinu('ZINTERSTORE', dstkey, keys, weights, aggregate,...)
}

redisZUnionStore <- function(dstkey, keys, weights=c(), aggregate=NULL,...)
{
  .zinu('ZUNIONSTORE', dstkey, keys, weights, aggregate,...)
}

redisSort <- function(key, decreasing=FALSE, alpha=FALSE,  by=NULL, start=NULL, 
                      count=NULL, get=NULL, store=NULL,...)
{
  a <- c(alist(), list(.raw('SORT')), list(.raw(key)),...)
  if(!is.null(by))
    a <- c(a, list(.raw('BY'), .raw(by)))
  if(!is.null(start) && !is.null(count))
    a <- c(a, list(.raw('LIMIT'), .raw(start), .raw(count)))
  if(!is.null(get))
    a <- c(a, list(.raw('GET'), .raw(get)))
  if(decreasing)
    a <- c(a, list(.raw('DESC')))
  else
    a <- c(a, list(.raw('ASC')))
  if(alpha)
    a <- c(a, list(.raw('ALPHA')))
  if(!is.null(store))
    a <- c(a, list(.raw('STORE'), .raw(store)))
  do.call('.redisCmd', a)
}
