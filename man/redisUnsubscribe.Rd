\name{redisUnsubscribe}
\alias{redisUnsubscribe}
\title{redisUnsubscribe}
\description{Subscribe to one or more Redis message channels.}
\usage{
redisUnsubscribe(channels, pattern=FALSE)
}
\arguments{
  \item{channels}{A character vector or list of channel names to subscribe to.}
  \item{pattern}{If TRUE, allow wildcard pattern matching in channel names,
                 otherwise names indicate full channel names.}
}
\details{(From the Redis.io documentation):
\code{redisSubscribe}, \code{redisUnsubscribe} and \code{redisPublish}
implement the Publish/Subscribe messaging paradigm where (citing Wikipedia)
senders (publishers) are not programmed to send their messages to specific
receivers (subscribers). Rather, published messages are characterized into
channels, without knowledge of what (if any) subscribers there may be.
Subscribers express interest in one or more channels, and only receive messages
that are of interest, without knowledge of what (if any) publishers there are.

Use the Redis function \code{redisUnsubscribe} to unsubscribe from one or
more channels. Service incoming messanges on the channels with either
\code{redisGetResponse} or \code{redisMonitorChannels}.

Use of any other Redis after \code{redisSubscribe} prior to calling
\code{redisUnsubscribe} will result in an error.
}
\value{A list conforming to the Redis subscribe response message.
Each subscribed channel corresponds to three list elements, the header
element 'unsubscribe' followed by the channel name and a count indicating
the total number of subscriptions.}
\references{
http://code.google.com/p/redis/
}
\author{
B. W. Lewis
}
\seealso{
\code{\link{redisSubscribe}}
\code{\link{redisUnsubscribe}}
\code{\link{redisPublish}}
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

