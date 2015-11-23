require(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()

  redisSetBit("bitmapr_test02", 10, 1)  
  checkEquals("1", redisGetBit("bitmapr_test02", 10))


  redisSetBit("bitmapr_test03", 1, 1)  
  redisSetBit("bitmapr_test03", 10, 1)  
  redisSetBit("bitmapr_test03", 100, 1)  
  checkEquals("3", redisBitCount("bitmapr_test03"))  

  sourcekeys <- c("bitmapr_test04_src_1", "bitmapr_test04_src_2","bitmapr_test04_src_3")
  
  redisSetBit(sourcekeys[1], 1, 1)
  redisSetBit(sourcekeys[1], 2, 1)
  redisSetBit(sourcekeys[1], 3, 1)
  redisSetBit(sourcekeys[1], 100, 1)
  
  redisSetBit(sourcekeys[2], 1, 1)
  redisSetBit(sourcekeys[2], 4, 1)
  redisSetBit(sourcekeys[2], 5, 1)
  redisSetBit(sourcekeys[2], 100, 1)
  
  redisSetBit(sourcekeys[3], 1, 1)
  redisSetBit(sourcekeys[3], 6, 1)
  redisSetBit(sourcekeys[3], 7, 1)
  redisSetBit(sourcekeys[3], 100, 1)
  
  operation <- "AND"
  destkey <- "bitmapr_test04_dest"
  
  redisBitOp(operation, destkey, sourcekeys)
  checkEquals("2", redisBitCount(destkey))
  checkEquals("0", redisGetBit(destkey, 5))
  checkEquals("1", redisGetBit(destkey, 1))

  redisFlushAll()
}
