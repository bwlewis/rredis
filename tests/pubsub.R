library(rredis)
checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

# redusMonitorChannels blocks forever until a message is received. We
# use a background R process to send us some test messages.
publisher <- function()

{
  Rbin <- paste(R.home(component="bin"), "R", sep="/")
  cmd <- "require(rredis);redisConnect();Sys.sleep(2);redisPublish('channel1', charToRaw('1'));redisPublish('channel2', charToRaw('2'))"
  args <- c("--slave", "-e", paste("\"", cmd, "\"", sep=""))
  system(paste(c(Rbin, args), collapse=" "), intern=FALSE, wait=FALSE)
}

if(Sys.getenv("RunRRedisTests") == "yes")
{
  redisConnect()
  redisFlushAll()
  redisPublish("channel1", charToRaw("A raw charachter data message example"))
  redisPublish("channel2", charToRaw("A raw charachter data message example"))

  # Define a callback function to process messages from channel 1:
  channel1 <- function(x) {
    cat("Message received from channel 1: ",x,"\n")
    checkEquals("1", x);
  }
  # Define a callback function to process messages from channel 2:
  channel2 <- function(x) {
    cat("Message received from channel 2: ",x,"\n")
    checkEquals("2", x);
  }
  redisSubscribe(c('channel1','channel2'))
  # Monitor channels for a few seconds until the background R process sends us
  # some messages...
  publisher()
  redisMonitorChannels()
  redisMonitorChannels()
  redisUnsubscribe(c('channel1','channel2'))

  redisFlushAll()
}
