require(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()

  redisZAdd("A", 1, "w")
  redisZAdd("A", 1, "x")
  redisZAdd("A", 2, "y")
  checkEquals("1", redisZAdd("A", 3, "z"))

  checkEquals("4", redisZCard("A"))

  checkEquals("1", redisZRem("A", "w"))

  checkEquals(2, as.integer(redisZIncrBy("A","x",1)))

  checkEquals("2", redisZRank("A", "z"))

  checkEquals(TRUE,all(c('y','z') == unlist(redisZRange("A",1,2))))

  checkEquals(TRUE,all(c('x','y') == redisZRangeByScore("A",min=0,max=2)))

  checkEquals("3", redisZUnionStore("B",c("A","A")))

  checkEquals("2",redisZRemRangeByScore("B",min=0,max=4))

  checkEquals("1", redisZInterStore("C",c("A","B")))

  checkEquals("1", redisZRemRangeByRank("B",0,1))

  checkEquals(TRUE, all(c("x","y","z") == unlist(redisSort("A",alpha=TRUE,decreasing=FALSE))))
  checkEquals(TRUE, all(c("z","y","x") == unlist(redisSort("A",alpha=TRUE,decreasing=TRUE))))

  redisFlushAll()
}
