redisEval <- function(script, keys=vector("list",0), SHA=FALSE, ...) {
  if(!is.list(keys)) keys = list(keys)
  numkeys = length(keys)
  if(numkeys>0) keys = as.character(keys)
  if(SHA)
    redisCmd("EVALSHA",script,as.character(numkeys),keys,...)
  else
    redisCmd("EVAL",script,as.character(numkeys),keys,...)
}
