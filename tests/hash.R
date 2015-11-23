require(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()

  x <- runif(5)
  redisHSet("A", "x", x)
  checkEquals(TRUE, redisHExists("A", "x"))
  checkEquals(x, redisHGet("A", "x"))

  redisHSet("A", "y", "1")
  checkEquals("2", redisHIncrBy("A", "y", "1"))
  checkEquals("3", redisHIncrByFloat("A", "y", "1"))

  redisHDel("A", "y")
  checkEquals(x, redisHGetAll("A")$x)
  checkEquals(x, redisHVals("A")[[1]])

  checkEquals("x", redisHFields("A")[[1]])
  checkEquals("1", redisHLen("A"))
  
  redisHMSet("A", list(y=pi, z=letters))
  checkEquals(list(x=x, z=letters), redisHMGet("A", c("x", "z")))

  redisFlushAll()
}
