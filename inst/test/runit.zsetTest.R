options('redis:num'=TRUE) # many tests assume : returns numeric

test01_connect <- function()
{
  redisConnect()
  redisFlushDB()
}

test02_redisZAdd <- function()
{
  redisZAdd("A", 1, "w")
  redisZAdd("A", 1, "x")
  redisZAdd("A", 2, "y")
  checkEquals(1, redisZAdd("A", 3, "z"))
}

test03_redisZCard <- function()
{
  checkEquals(4, redisZCard("A"))
}

test04_redisZRem <- function()
{
  checkEquals(1, redisZRem("A","w"))
}

test05_redisZIncrBy <- function()
{
  checkEquals(2, as.integer(redisZIncrBy("A","x",1)))
}

test06_redisZRank <- function()
{
  checkEquals(as.numeric(2), redisZRank("A","z"))
}

test08_redisZRange <- function()
{
  checkEquals(TRUE,all(c('y','z')==unlist(redisZRange("A",1,2))))
}

test10_redisZRangeByScore <- function()
{
  checkEquals(TRUE,all(c('x','y')==redisZRangeByScore("A",min=0,max=2)))
}

test11_redisZUnionStore <- function()
{
  checkEquals(3, redisZUnionStore("B",c("A","A")))
}

test12_redisZRemRangeByScore <- function()
{
  checkEquals(2,redisZRemRangeByScore("B",min=0,max=4))
}

test13_redisZInterStore <- function()
{
  checkEquals(1, redisZInterStore("C",c("A","B")))
}

test14_redisZRemRangeByRank <- function()
{
  checkEquals(1, redisZRemRangeByRank("B",0,1))
}
