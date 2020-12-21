# *IMPORTANT NOTICE*

The rredis is defunct and I will not continue developing it.  I urge all rredis
users to switch to the **redux** package, https://github.com/richfitz/redux
(also on CRAN). The redux package provides a more complete interface to Redis,
with a much better (consistent, flexible, simpler) internal design.

It's easy to convert projects that rely on rredis to use redux. See the
examples below.

I'll keep rredis around for a long time until I'm sure most folks that depend
on it have moved over to redux, so there is no great urgency to switch. But
redux is better and you should use it!

This package will be removed from CRAN soon, so switch to the redux package please.

# rredis: An R client for Redis

## Example

```R
library(rredis)
redisConnect()
redisSet('foo', runif(10))
bar <- redisGet('foo')
bar
# [1] 0.93499818 0.47159536 0.30597259 0.58325228 0.41589498 0.63914212 0.34658694 0.08633471 0.18111369 0.15763507

redisMSet(list(x=pi,y='Cazart',z=runif(2)))
redisMGet(list('z','y'))
#$z
#[1] 0.1155711 0.7166137
#
#$y
#[1] "Cazart"

redisClose()
```


### Use `redisCmd` to run any Redis command

This is more or less how the `redux` package works.

Some Redis commands have corresponding convenience wrappers with online
help in the R package. Use the generic `redisCmd` function to run _any_
Redis command, even ones not specifically implemented by the package.
For example:

```r
redisCmd("set","key1","foobar")
redisCmd("set","key2","abcdef")
redisCmd("bitop", "and", "dest", "key1", "key2")
#  [1] "6"
redisCmd("get", "dest")
#  [1] "`bc`ab"

# in redux:
library(redux)
con <- hiredis()
con$set("key1", "foobar")
con$get("key1")
# [1] "foobar"

# setting/getting a serialized R value in redux
con$set("key2", serialize(pi, NULL))
unserialize(con$get("key2"))
# [1] 3.14159
```

## New in version 1.7.0

### Better value exchange between R and Redis

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

### API change, and option to revert behavior

Set `options('redis:num'=TRUE)` to return
Redis "`:`" messages as numeric values. This was the default behavior
of the rredis package for all versions prior to 1.6.9. For versions
of the R package later than that, redis "`:`" messages are returned
as raw Redis string values to correspond to the data types stored in Redis.
Set this option to revert to the old behavior.

Redis commands affected by this option importantly include the increment
and decrement operations. This change is outlined in the following example:
```
> library(rredis)
> redisConnect()
> redisSet('x',charToRaw('1'))
[1] "OK"

> redisIncr('x')
[1] "2"
attr(,"redis string value")
[1] TRUE

> options('redis:num'=TRUE)
> redisIncr('x')
[1] 3

> options('redis:num'=c())
> redisIncr('x')
[1] "4"
attr(,"redis string value")
[1] TRUE
```


## Performance

Consider using the redisSetPipeline function to enable pipelining, and also
read help about options available to the redisConnect function.  Also see the
available options in the redisConnect function.

## Status
<a href="https://travis-ci.org/bwlewis/rredis">
<img src="https://travis-ci.org/bwlewis/rredis.svg?branch=master" alt="Travis CI status"></img>
</a>
[![codecov.io](https://codecov.io/github/bwlewis/rredis/coverage.svg?branch=master)](https://codecov.io/github/bwlewis/rredis?branch=master)
[![CRAN version](http://www.r-pkg.org/badges/version/rredis)](http://cran.rstudio.com/web/packages/rredis/index.html)
