library(rredis)
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
}
