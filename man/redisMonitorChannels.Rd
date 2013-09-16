\name{redisMonitorChannels}
\alias{redisMonitorChannels}
\title{redisMonitorChannels}
\description{Subscribe to one or more Redis message channels.}
\usage{
redisMonitorChannels()
}
\details{(From the Redis.io documentation):
\code{redisSubscribe}, \code{redisUnsubscribe} and \code{redisPublish}
implement the Publish/Subscribe messaging paradigm where (citing Wikipedia)
senders (publishers) are not programmed to send their messages to specific
receivers (subscribers). Rather, published messages are characterized into
channels, without knowledge of what (if any) subscribers there may be.
Subscribers express interest in one or more channels, and only receive messages
that are of interest, without knowledge of what (if any) publishers there are.

The \code{redisMonitorChannels} function may be called repeatedly in an
event loop to service messages on all subscribed channels. When a message
is received, the \code{redisMonitorChannels} function will attempt to
evaluate a callback function with same name as the channel, with the message
as its single argument. If no such function can be found, the message is
returned. See the help page for \code{redisGetResponse} for a description
of the message format.

WARNING: The \code{redisMonitorChannels} function blocks indefinitely until a 
message is received.

Use the lower-level \code{redisGetResponse} function to simply poll channels
for messages without evaluating function callbacks.
}
\value{The result of an evaluated function callback message, or if
no matching callback exists, the message.}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSubscribe}}
\code{\link{redisPublish}}
\code{\link{redisUnsubscribe}}
\code{\link{redisGetResponse}}

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
# Monitor channels for at least 1 minute:
t1 <- proc.time()[[3]]
while(proc.time()[[3]] - t1 < 60)
{
  redisMonitorChannels()
  Sys.sleep(0.05)
}
redisUnsubscribe(c('channel1','channel2'))
}
}

