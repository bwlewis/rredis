# rredis: An R client for Redis

## Example

```R
> library(rredis)
> redisConnect()         
> redisSet('foo', runif(10))
> bar <- redisGet('foo') 
> bar
 [1] 0.93499818 0.47159536 0.30597259 0.58325228 0.41589498 0.63914212
 [7] 0.34658694 0.08633471 0.18111369 0.15763507

> redisMSet(list(x=pi,y='Cazart',z=runif(2)))
> redisMGet(list('z','y'))
$z
[1] 0.1155711 0.7166137

$y
[1] "Cazart"
```

## New in version 1.6.10

We implemented a great suggestion by Simon Urbanek. Values obtained from Redis
that are *not* serialized R objects are now decorated with an attribute named
"redis string value." The package uses this to automatically maintain fidelity
of the original Redis value through repeated download/upload cycles. Previous
versions of the rredis package uploaded everything as serialized R values
unless explictly told otherwise.

Consider the following interplay between the `redis-cli` client and R:

```
redis-cli set key "string value"
```
And now in R:
```r
> library(rredis)
> redisConnect()
> redisGet("key")
[1] "string value"
attr(,"redis string value")     # <- note the new attribute
[1] TRUE

> redisSet("new key", redisGet("key"))
```
Recovering the "new key" value from the `redis-cli` client returns a string
value now:
```
redis-cli get "new key"
"string value"
```
Before this change, users needed to be careful about converting strings to
raw values in R. Now things work much more intuitively.


## Performance

Consider using the redisSetPipeline function to enable pipelining, and also
read help about options available to the redisConnect function.  Also see the
available options in the redisConnect function.

## Status
<a href="https://travis-ci.org/bwlewis/rredis">
<img src="https://travis-ci.org/bwlewis/rredis.svg?branch=master" alt="Travis CI status"></img>
</a>
