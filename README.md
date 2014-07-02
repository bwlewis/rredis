rredis: An R client for Redis

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

> redisClose()
```

## Performance

Consider using the redisSetPipeline function to enable pipelining, and also
read help about options available to the redisConnect function.  Also see the
available options in the redisConnect function.

## Status
<a href="https://travis-ci.org/bwlewis/rredis">
<img src="https://travis-ci.org/bwlewis/rredis.svg?branch=master" alt="Travis CI status"></img>
</a>
