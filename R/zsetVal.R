# Redis ordered set functions

redisZAdd <- function(key, score, member)
{
  .redisCmd(.raw('ZADD'),.raw(set), .raw(score), member)
}

redisZRem <- function(key, member)
{
  .redisCmd(.raw('ZREM'),.raw(key), member)
}

redisZIncrBy <- function(key, member, increment)
{
  .redisCmd(.raw('ZREM'),.raw(key), .raw(increment), member)
}

redisZRank <- function(key, member)
{
  .redisCmd(.raw('ZRANK'),.raw(key), member)
}

redisZRevRank <- function(key, member)
{
  .redisCmd(.raw('ZREVRANK'),.raw(key), member)
}

redisZRange <- function(key, start, end, withscores=FALSE)
{
  if(withscores)
    .redisCmd(.raw('ZRANGE'),.raw(key),.raw(start),.raw(end),.raw('WITHSCORES'))
  else
    .redisCmd(.raw('ZRANGE'),.raw(key),.raw(start),.raw(end))
}

redisZRevRange <- function(key, start, end, withscores=FALSE)
{
  if(withscores)
    .redisCmd(.raw('ZREVRANGE'),.raw(key),.raw(start),.raw(end),.raw('WITHSCORES'))
  else
    .redisCmd(.raw('ZREVRANGE'),.raw(key),.raw(start),.raw(end))
}

redisZRangeByScore <- function(key, min, max, offset=NULL, count=NULL, withscores=FALSE)
{
  a <- c(alist(), list(.raw('ZRANGEBYSCORE'), .raw(key), .raw(min), .raw(max)))
  if(!is.null(offset) && !is.null(count)) {
    a <- c(a, list(.raw('LIMIT'), .raw(offset), .raw(count)))
  }
  if(withscores)
    a <- c(a, as.list(.raw('WITHSCORES')))
  do.call('.redisCmd', a)
}

redisZRemRangeByRank <- function(key, start, end)
{
  .redisCmd(.raw('ZREMRANGEBYRANK'), .raw(key), .raw(start), .raw(end))
}

redisZRemRangeByScore <- function(key, min, max)
{
  .redisCmd(.raw('ZREMRANGEBYRANK'), .raw(key), .raw(min), .raw(max))
}

redisZCard <- function(key)
{
  .redisCmd(.raw('ZCARD'), .raw(key))
}

redisZScore <- function(key, element)
{
  ret <- .redisCmd(.raw('ZSCORE'), .raw(key), .raw(element))
  if(!is.null(ret)) ret <- as.numeric(ret)
  ret
}

.zinu <- function(type, dstkey, keys, weights=c(), aggregate=NULL)
{
  a <- c(alist(), list(.raw(type), .raw(dstkey)))
  sets <- lapply(as.list(keys),charToRaw)
  a <- c(a, sets)
  if(!is.null(weights)) {
    a <- c(a, .raw('WEIGHTS'), lapply(as.list(weights), charToRaw))
  }
  if(!is.null(aggregate)) {
    a <- c(a, .raw('AGGREGATE'), .raw(aggregate))
  }
  do.call('.redisCmd', a)
}

redisZInter <- function(dstkey, keys, weights=c(), aggregate=NULL)
{
  .zinu('ZINTER', dstkey, keys, weights, aggregate)
}

redisZUnion <- function(dstkey, keys, weights=c(), aggregate=NULL)
{
  .zinu('ZUNION', dstkey, keys, weights, aggregate)
}

redisSort <- function(key, decreasing=FALSE, alpha=FALSE,  by=NULL, start=NULL, 
                      count=NULL, get=NULL, store=NULL)
{
  a <- c(alist(), list(.raw(key)))
  if(!is.null(by))
    a <- c(a, as.list(.raw('BY'), .raw(by)))
  if(!is.null(start) && !is.null(count))
    a <- c(a, as.list(.raw('LIMIT'), .raw(start), .raw(count)))
  if(!is.null(get))
    a <- c(a, as.list(.raw('GET'), .raw(get)))
  if(decreasing)
    a <- c(a, as.list(.raw('DESC')))
  else
    a <- c(a, as.list(.raw('ASC')))
  if(alpha)
    a <- c(a, as.list(.raw('ALPHA')))
  if(store)
    a <- c(a, as.list(.raw('STORE'), .raw(store)))
  do.call('.redisCmd', a)
}
