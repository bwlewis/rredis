\name{redisBgRewriteAOF}
\alias{redisBgRewriteAOF}
\title{redisBgRewriteAOF}
\description{
Re-write the Redis append-only file in the background.
}
\usage{
redisBgRewriteAOF()
}
\details{
BGREWRITEAOF rewrites the Append Only File in the background when it gets too
big.  The Redis Append Only File is a Journal, so every operation modifying the
dataset is logged in the Append Only File (and replayed at startup). This means
that the Append Only File always grows. In order to rebuild its content the
BGREWRITEAOF creates a new version of the append only file starting directly
form the dataset in memory in order to guarantee the generation of the minimal
number of commands needed to rebuild the database.
(These details are copied verbatim from the Redis master documentation, see the
references below.)
}
\value{
Nothing is returned.
}
\references{
http://redis.io/commands
}
\author{
B. W. Lewis
}
