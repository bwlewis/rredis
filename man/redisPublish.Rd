\name{redisPublish}
\alias{redisPublish}
\title{redisPublish}
\description{Publish data to a Redis message channel.}
\usage{
redisPublish(channel, message)
}
\arguments{
  \item{channel}{The channel name (character).}
  \item{message}{The message (any type, use RAW type for communication with
                 non-R subscribers.}
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
'subscribe' followed by the channel name and a count indicating the total
number of subscriptions.}
\references{
http://redis.io/commands
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
redisPublish('channel1', charToRaw('A raw charachter data message example'))
}
}
