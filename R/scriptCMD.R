redisEval <- function(script, keys=vector("list",0), SHA=FALSE, ...)
{
  if(!is.list(keys)) keys = list(keys)
  numkeys = length(keys)
  if(numkeys>0) keys = as.character(keys)
  CMD = ifelse(SHA,"EVALSHA","EVAL")
  do.call(CMD, args=c(list(script, as.character(numkeys)),keys,list(...)))
}
