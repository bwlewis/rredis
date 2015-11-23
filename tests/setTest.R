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

  redisSInterStore("C", c("A", "B"))
  checkEquals(TRUE, all(2 %in% redisSMembers("C")))

  checkEquals(TRUE, redisSIsMember("A", 3))
  checkEquals(2, redisSRandMember("B"))

  checkEquals(TRUE, all(list(1, 2, 3) %in% redisSUnion("A", "B")))
  redisSUnionStore("C", "A", "B")
  checkEquals(TRUE, all(list(1, 2, 3) %in% redisSMembers("C")))

  redisSUnionStore("C", c("A", "B"))  # Alternate form
  checkEquals(TRUE, all(list(1, 2, 3) %in% redisSMembers("C")))

  checkEquals(TRUE, all(list(1, 3) %in% redisSDiff("A","B")))
  redisSDiffStore("C", "A", "B")
  checkEquals(TRUE, all(list(1, 3) %in% redisSMembers("C")))

  redisSRem("A", 1)
  checkEquals("2", redisSCard("A"))

  redisSPop("A")
  checkEquals("1", redisSCard("A"))

  x <- redisSMembers("A")[[1]]
  redisSMove("A", "B", x)
  checkEquals(TRUE, all(x %in% redisSMembers("B")))

  redisFlushAll()
}
