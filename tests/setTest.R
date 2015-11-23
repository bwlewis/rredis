require(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()

  redisSAdd("A",1)
  redisSAdd("A",2)
  redisSAdd("A",3)
  checkEquals("3", redisSCard("A"))

  redisSAdd("B",2)
  checkEquals(2, redisSInter("A","B")[[1]])

  checkEquals(TRUE, all(list(1, 2, 3) %in% redisSUnion("A", "B")))
  redisSUnionStore("C", "A", "B")
  checkEquals(TRUE, all(list(1, 2, 3) %in% redisSMembers("C")))

  redisSUnionStore("C", c("A", "B"))  # Alternate form
  checkEquals(TRUE, all(list(1, 2, 3) %in% redisSMembers("C")))

  checkEquals(list(1, 3), redisSDiff("A","B"))
  redisSDiffStore("C", "A", "B")
  checkEquals(TRUE, all(list(1, 3) %in% redisSMembers("C")))

  redisSRem("A", 1)
  checkEquals("2", redisSCard("A"))

  redisSPop("A")
  checkEquals("1", redisSCard("A"))

  redisFlushAll()
}
