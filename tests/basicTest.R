library(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()
  checkEquals(FALSE, redisExists("foo"))

  # delete test
  checkEquals("0", suppressWarnings(redisDelete("foo")))

  # empty get test
  checkEquals(TRUE, is.null(redisGet("foo")))

  # simple set test
  checkEquals("OK", redisSet("foo", "bar"))

  # simple getset test
  checkEquals("bar", redisGetSet("foo", "zip"))

  # getset confirm
  checkEquals("zip", redisGet("foo"))

  # set/serialize test
  foo <- runif(10)
  redisSet("foo", foo)
  checkEquals(foo, redisGet("foo"))

  # simple type check
  checkEquals("string", redisType("foo"))

  # mget test
  redisSet("foo", "bar")
  redisSet("bar", "foo")
  checkEquals(list(foo="bar", bar="foo"), redisMGet(c("foo", "bar")))

  # simple mset test
  checkEquals("OK", redisMSet(list(foo="foo",bar="bar")))

  # simple mset confirm
  checkEquals("foo", redisGet("foo"))

  # real mset test
  redisDelete(c("foo", "bar"))
  redisMSet(list(foo="bar",bar="foo"))
  checkEquals(list(foo="bar",bar="foo"), redisMGet(c("foo", "bar")))
  redisDelete(c("foo", "bar"))

  # real exists test
  checkEquals(FALSE, redisExists("foo"))
  redisSet("foo", 1)
  checkEquals(TRUE, redisExists("foo"))
  redisDelete("foo")

  # expire test
  redisSet("foo", 1)
  redisExpire("foo", 1)
  Sys.sleep(2)
  checkEquals(FALSE, redisExists("foo"))

  # rename/renamenx test
  redisSet("foo", 1)
  redisSet("bar", 2)
  redisRename("foo", "bar")
  checkEquals(1, redisGet("bar"))
  redisSet("foo", 2)
  checkEquals("0", redisRename("foo", "bar", NX=TRUE))
  redisDelete("bar")
  redisRename("foo", "bar", NX=TRUE)
  checkEquals(2, redisGet("bar"))

  # set/mset nx mode test
  redisFlushAll()
  checkEquals("1", redisSet("foo", 1, NX=TRUE))
  checkEquals("0", redisSet("foo", 1, NX=TRUE))
  checkEquals("0", redisMSet(list(foo=1), NX=TRUE))
  redisDelete("foo")
  checkEquals("1", redisMSet(list(foo=1,bar=2), NX=TRUE))

  redisFlushAll()
}
