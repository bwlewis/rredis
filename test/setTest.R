
test01_connect <- function()
{
  redisConnect()
  redisFlushDB()
}

test02_redisSAdd_Inter <- function()
{
  redisSAdd("A",1)
  redisSAdd("A",2)
  redisSAdd("A",3)
  redisSAdd("B",2)
  checkEquals(2, redisSInter("A","B")[[1]])
}

test03_redisSUnion <- function()
{
  checkEquals(TRUE, all(list(1,2,3) %in% redisSUnion("A","B")))
}

test04_redisSCard <- function()
{
  checkEquals(3, redisSCard("A"))
}
