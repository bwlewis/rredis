test01 <- function() {
  redisConnect()
}

test02 <- function() {
  # legacy exists test
  checkEquals(FALSE, redisExists('foo'))
}

test03 <- function() {
  # delete test
  checkEquals(FALSE, suppressWarnings(redisDelete('foo')))
}

test04 <- function() {
  # empty get test
  checkTrue(is.null(redisGet('foo')))
}

test05 <- function() {
  # simple set test
  checkEquals('OK', redisSet('foo', 'bar'))
}

test06 <- function() {
  # simple getset test
  checkEquals('bar', redisGetSet('foo', 'zip'))
}

test07 <- function() {
  # getset confirm
  checkEquals('zip', redisGet('foo'))
}

test08 <- function() {
  # set/serialize test
  foo <- runif(10)
  redisSet('foo', foo)
  checkEqualsNumeric(foo, redisGet('foo'))
}

test09 <- function() {
  # simple type check
  checkEquals('string', redisType('foo'))
}

test10 <- function() {
  # mget test
  redisSet('foo', 'bar')
  redisSet('bar', 'foo')
  checkEquals(list('bar', 'foo'), redisMGet(c('foo', 'bar')))
}

test11 <- function() {
  # simple mset test
  checkEquals('OK', redisMSet(c('foo'), c('foo')))
}

test12 <- function() {
  # simple mset confirm
  checkEquals('foo', redisGet('foo'))
}

test13 <- function() {
  # real mset test
  redisDelete(c('foo', 'bar'))
  redisMSet(c('foo', 'bar'), c('foo','bar'))
  checkEquals(list('foo', 'bar'), redisMGet(c('foo', 'bar')))
  redisDelete(c('foo', 'bar'))
}

test14 <- function() {
  # real exists test
  checkEquals(FALSE, redisExists('foo'))
  redisSet('foo', 1)
  checkTrue(redisExists('foo'))
  redisDelete('foo')
}

test15 <- function() {
  # keys test
  checkEquals('', redisKeys('*'))
  redisSet('foo', 1)
  checkEquals('foo', redisKeys('*'))
  redisDelete('foo')
}

test16 <- function() {
  # randomkey test
  checkEquals('', redisRandomKey())
  redisSet('foo', 1)
  checkEquals('foo', redisRandomKey())
  redisDelete('foo')
}

test17 <- function() {
  # expire test
  redisSet('foo', 1)
  redisExpire('foo', 1)
  Sys.sleep(2)
  checkEquals(FALSE, redisExists('foo'))
  # expireat not tested because I don't know how
  # to get unix time in R. -PS
}

test18 <- function() {
  # rename/renamenx test
  redisSet('foo', 1)
  redisSet('bar', 2)
  redisRename('foo', 'bar')
  checkEquals(1, redisGet('bar'))
  redisSet('foo', 2)
  checkEquals(FALSE, redisRenameNX('foo', 'bar'))
  redisDelete('bar')
  redisRenameNX('foo', 'bar')
  checkEquals(2, redisGet('bar'))
}

test98 <- function() {
  suppressWarnings(redisDelete(c('foo','bar')))
}

test99 <- function() {
  redisClose()
}
