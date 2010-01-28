test01 <- function() {
  redisConnect()
}

test02 <- function() {
  checkEquals(FALSE, redisExists('foo'))
}

test03 <- function() {
  checkEquals(FALSE, redisDelete('foo'))
}

test04 <- function() {
  checkTrue(is.null(redisGet('foo')))
}

test05 <- function() {
  checkEquals('OK', redisSet('foo', 'bar'))
}

test05 <- function() {
  checkEquals('bar', redisGetSet('foo', 'zip'))
}

test06 <- function() {
  checkEquals('zip', redisGet('foo'))
}

test07 <- function() {
  foo <- runif(10)
  redisSet('foo', foo)
  checkEqualsNumeric(foo, redisGet('foo'))
}

test99 <- function() {
  redisClose()
}
