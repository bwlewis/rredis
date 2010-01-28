redisConnect <-
function(host='localhost', port=6379, returnRef=FALSE)
{
  con <- socketConnection(host, port,open='a+b')
# Stash state in the redis enivronment describing this connection:
  assign('con',con,envir=.redisEnv)
  assign('host',host,envir=.redisEnv)
  assign('port',port,envir=.redisEnv)
  tryCatch(.redisPP(), 
    error=function(e) {
      cat(paste('Error: ',e,'\n'))
            close(con);
            rm(list='con',envir=.redisEnv)
          })
  if(returnRef) return(list(con=con,host=host,port=port))
  invisible()
}

