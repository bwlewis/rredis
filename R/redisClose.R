redisClose <-
function()
{
  con <- .redis()
  close(con)
  remove(list='con',envir=.redisEnv)
}

