#' @title Redis BITSET - set binary value
#' @description Sets or clears the bit at offset in the string value stored at key.
#' @param key redis key
#' @param offset integer index to be updated
#' @param bit binary number to be set
#' @return bit binary number with previous value, or '0' if it had not been set before.
#' @detail Official redis documentation: http://redis.io/commands/setbit
redisSetBit <- function(key, offset, bit)
{
  .redisCmd(.raw('SETBIT'), .raw(key), .raw(as.character(offset)), .raw(as.character(bit)))
}


#' @title Redis BITSET gets - get binary value
#' @description Returns the bit value at offset in the string value stored at key.
#' @param key redis key
#' @param offset integer index
#' @return bit binary integer
#' @detail Official redis documentation: http://redis.io/commands/getbit
redisGetBit <- function(key, offset)
{
  .redisCmd(.raw('GETBIT'), .raw(key), .raw(as.character(offset)))
}


#' @title Redis BITCOUNT - count all bits in key 
#' @description Count the number of set bits (population counting) in a string.
#' @param key redis key
#' @return the counted bits as an integer value
#' @detail Official redis documentation: http://redis.io/commands/bitcount
redisBitCount <- function(key)
{
  .redisCmd(.raw('BITCOUNT'), .raw(key))
}


#' @title Redis BITOP - execute bitoperations on multiple bitsets 
#' @description Perform a bitwise operation between multiple keys (containing string values) and store the result in the destination key
#' @param operation bit operation as character: 'AND', 'OR', 'XOR', 'NOT'
#' @param destkey destination key where the resulting bit operation will be stored 
#' @param sourcekeys one or more source keys subject to the bit operations
#' @return the counted bits as an integer value
#' @detail Official redis documentation: http://redis.io/commands/bitop
redisBitOp <- function(operation, destkey, sourcekeys)
{
  sets <- c(as.list(sourcekeys))
  do.call('.redisCmd',lapply(c(list('BITOP'),operation, destkey, sets),charToRaw))
}
