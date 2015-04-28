test98_flush <- function() {
  suppressWarnings(redisFlushAll())
}

test99_close <- function() {
  redisClose()
}
