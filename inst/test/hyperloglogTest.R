test01_connect <- function()
{
  redisConnect()
  redisFlushDB()
}

test02_redisPfadd <- function()
{
  redisPfadd("testcounter", letters[1:10])
  redisPfadd("testcounter", letters[1:20])
  redisPfcount("testcounter")
  redisPfadd("testcounter_2", "z")
  redisPfmerge("testcounter", "testcounter_2")
  x = as.integer(redisPfcount("testcounter"))
# Note! HyperLogLog is only an approximate count!
  checkEquals(TRUE, x > 10 && x < 30)
}
