\name{redisGetResponse}
\alias{redisGetResponse}
\title{redisGetResponse}
\description{Subscribe to one or more Redis message channels.}
\usage{
redisGetResponse()
}
\details{(From the Redis.io documentation):
\code{redisSubscribe}, \code{redisUnsubscribe} and \code{redisPublish}
implement the Publish/Subscribe messaging paradigm where (citing Wikipedia)
senders (publishers) are not programmed to send their messages to specific
receivers (subscribers). Rather, published messages are characterized into
channels, without knowledge of what (if any) subscribers there may be.
Subscribers express interest in one or more channels, and only receive messages
that are of interest, without knowledge of what (if any) publishers there are.

The \code{redisGetResponse} function may be called 
to service messages on all subscribed channels. When a message
is received the a list of three elements is returned containing:
the character string 'message', the name of receiving channel, and
the message content.

WARNING: The \code{redisGetResponse} function blocks indefinitely until a 
message is received.
}
\value{A list containing the string 'message', the channel name from which
the message was received, and the message data.}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSubscribe}}
\code{\link{redisPublish}}
\code{\link{redisUnsubscribe}}
\code{\link{redisMonitorChannels}}
}
\examples{
\dontrun{
redisConnect()
# Define a callback function to process messages from channel 1:
channel1 <- function(x) {
  cat("Message received from channel 1: ",x,"\n")
}
# Define a callback function to process messages from channel 2:
channel2 <- function(x) {
  cat("Message received from channel 2: ",x,"\n")
}
redisSubscribe(c('channel1','channel2'))
# Monitor channels for at least one minute (and indefinitely until
# a message is received):
t1 <- proc.time()[[3]]
while(proc.time()[[3]] - t1 < 60)
{
  print(redisGetResponse())
}
redisUnsubscribe(c('channel1','channel2'))
}
}
