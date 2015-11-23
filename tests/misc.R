require(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()

  redisSetPipeline(TRUE)
  redisMulti()
  redisSet("x", pi)
  redisExec()
  redisSetPipeline(FALSE)
  redisGetResponse()
  checkEquals(pi, redisGet("x"))

  checkEquals("x", redisKeys())
  checkEquals("x", redisRandomKey())

  redisPexpire("x", 1000)
  redisPTTL("x")
  redisPersist("x")
  
  redisPexpireAt("x", 1000*(as.numeric(format(Sys.time(), "%s")) + 10))
  redisPersist("x")
  redisExpireAt("x", as.numeric(format(Sys.time(), "%s")) + 10)
  redisTTL("x")
  redisPersist("x")
  redisExpire("x", 1)

# Need to trap this as it depends on external Redis configuration and could
# fail simply due to that
  tryCatch({
    redisSet("y", pi)
    redisMove("y", 1)
  }, error=invisible)

  redisSave()
  redisBgSave()
  redisBgRewriteAOF()

  redisInfo()
  redisDBSize()
  redisFlushDB()

  e <- redisGetContext()
  redisSetContext(e)
  redisSet("x", 1)

  f <- redisConnect(returnRef=TRUE, closeExisting=FALSE)
  redisSet("x", 1)
  redisSetContext(e)

  checkEquals(1, redisGet("x"))

  redisClose(e)
  redisClose(f)

  redisConnect()
  redisFlushAll()


  redisWatch("x")
  redisMulti()
  redisSet("x", 1)
  checkEquals("OK", redisDiscard())

  redisUnwatch()
  checkEquals(FALSE, redisExists("x"))

}
