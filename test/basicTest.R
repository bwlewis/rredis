test01 <- function() {
  redisConnect()
}

test02 <- function() {
  checkEquals(FALSE, redisExists('foo'))
}

test03 <- function() {
  checkEquals(FALSE, suppressWarnings(redisDelete('foo')))
}

test04 <- function() {
  checkTrue(is.null(redisGet('foo')))
}

test05 <- function() {
  checkEquals('OK', redisSet('foo', 'bar'))
}

test06 <- function() {
  checkEquals('bar', redisGetSet('foo', 'zip'))
}

test07 <- function() {
  checkEquals('zip', redisGet('foo'))
}

test08 <- function() {
  foo <- runif(10)
  redisSet('foo', foo)
  checkEqualsNumeric(foo, redisGet('foo'))
}

test09 <- function() {
  checkEquals('string', redisType('foo'))
}

test10 <- function() {
  redisSet('foo', 'bar')
  redisSet('bar', 'foo')
  checkEquals(list('bar', 'foo'), redisMGet(c('foo', 'bar')))
}

#test11 <- function() {
#  redisMSet(c('foo', 'bar'), c('foo','bar'))
#  checkEquals(list('foo', 'bar'), redisMGet(c('foo', 'bar')))
#}

test11 <- function() {
  checkEquals('OK', redisMSet(list('foo'), list('bar')))
}

test12 <- function() {
  print(redisGet('foo'))
  checkEquals('bar', redisGet('foo'))
}

test98 <- function() {
  #redisDelete('foo')
  redisDelete('bar')
}

test99 <- function() {
  redisClose()
}
