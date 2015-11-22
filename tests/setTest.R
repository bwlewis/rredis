library(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()

  redisSAdd("A",1)
  redisSAdd("A",2)
  redisSAdd("A",3)
  redisSAdd("B",2)
  checkEquals(2, redisSInter("A","B")[[1]])

  checkEquals(TRUE, all(list(1,2,3) %in% redisSUnion("A","B")))

  checkEquals("3", redisSCard("A"))

  redisFlushAll()
}
