test01_connect <- function() {
  redisConnect()
}

test02_exists <- function() {
  # legacy exists test
  redisFlushAll()
  checkEquals(FALSE, redisExists('foo'))
}

test03_delete <- function() {
  # delete test
  redisFlushAll()
  checkEquals(FALSE, suppressWarnings(redisDelete('foo')))
}

test04_empty_get <- function() {
  # empty get test
  redisFlushAll()
  checkTrue(is.null(redisGet('foo')))
}

test05_set <- function() {
  # simple set test
  checkEquals(TRUE, redisSet('foo', 'bar'))
}

test06_getset <- function() {
  # simple getset test
  checkEquals('bar', redisGetSet('foo', 'zip'))
}

test07_get <- function() {
  # getset confirm
  checkEquals('zip', redisGet('foo'))
}

test08_serialization <- function() {
  # set/serialize test
  foo <- runif(10)
  redisSet('foo', foo)
  checkEqualsNumeric(foo, redisGet('foo'))
}

test09_type <- function() {
  # simple type check
  checkEquals('string', redisType('foo'))
}

test10_mget <- function() {
  # mget test
  redisSet('foo', 'bar')
  redisSet('bar', 'foo')
  checkEquals(list(foo='bar', bar='foo'), redisMGet(c('foo', 'bar')))
}

test11_mset_simple <- function() {
  # simple mset test
  checkEquals(TRUE, redisMSet(list(foo='foo',bar='bar')))
}

test12_get <- function() {
  # simple mset confirm
  checkEquals('foo', redisGet('foo'))
}

test13_mset <- function() {
  # real mset test
  redisDelete(c('foo', 'bar'))
  redisMSet(list(foo='bar',bar='foo'))
  checkEquals(list(foo='bar',bar='foo'), redisMGet(c('foo', 'bar')))
  redisDelete(c('foo', 'bar'))
}

test14_exists <- function() {
  # real exists test
  checkEquals(FALSE, redisExists('foo'))
  redisSet('foo', 1)
  checkTrue(redisExists('foo'))
  redisDelete('foo')
}

###test15 <- function() {
###  # keys test
###  redisFlushAll()
###  checkEquals(NULL, redisKeys('*'))
###  redisSet('foo', 1)
###  checkEquals(list('foo'), redisKeys('*'))
###  redisDelete('foo')
###}

###test16 <- function() {
###  # randomkey test
###  checkEquals('', redisRandomKey())
###  redisSet('foo', 1)
###  checkEquals('foo', redisRandomKey())
###  redisDelete('foo')
###}

test17_expire <- function() {
  # expire test
  redisSet('foo', 1)
  redisExpire('foo', 1)
  Sys.sleep(2)
  checkEquals(FALSE, redisExists('foo'))
  # expireat not tested because I don't know how
  # to get unix time in R. -PS
}

test18_rename <- function() {
  # rename/renamenx test
  redisSet('foo', 1)
  redisSet('bar', 2)
  redisRename('foo', 'bar')
  checkEquals(1, redisGet('bar'))
  redisSet('foo', 2)
  checkEquals(FALSE, redisRename('foo', 'bar', NX=TRUE))
  redisDelete('bar')
  redisRename('foo', 'bar', NX=TRUE)
  checkEquals(2, redisGet('bar'))
}

test19_setmsetnx <- function() {
  # set/mset nx mode test
  redisFlushAll()
  checkTrue(redisSet('foo', 1, NX=TRUE))
  checkEquals(FALSE, redisSet('foo', 1, NX=TRUE))
  checkEquals(FALSE, redisMSet(list(foo=1), NX=TRUE))
  redisDelete('foo')
  checkTrue(redisMSet(list(foo=1,bar=2), NX=TRUE))
  redisDelete(c('foo','bar'))
}

