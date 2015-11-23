require(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()

  redisLPush("x", 1)
  redisRPush("x", 2)
  checkEquals("2", redisLLen("x"))
  checkEquals(list(1,2), redisLRange("x", 0, 2))
  redisLSet("x", 0, pi)
  checkEquals(pi, redisLPop("x"))
  redisRPush("x", 2)
  redisLTrim("x", 0, 0)
  checkEquals("1", redisLLen("x"))
  checkEquals(2, redisLIndex("x", 0))
  redisLRem("x", 1, 2)
  checkEquals("0", redisLLen("x"))

  redisLPush("x", 1)
  redisRPush("x", 2)
  checkEquals(list(1,2), redisLRange("x", 0, 2))
  redisBRPopLPush('x','x')
  checkEquals(list(2,1), redisLRange("x", 0, 2))
  redisRPopLPush('x','x')
  redisBLPop("x")
  redisBRPop("x")
  checkEquals("0", redisLLen("x"))

  redisFlushAll()
}
