## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Course 2: R Programming, Week 3 
# Student: Jose Calvo

# This assignment is for creating a pair of functions in order to calculate and cache the inverse of a matrix

# MakeCacgeMatrix is a function that will be saving a matrix into a List, which will be used later to get the values of the matrix and GET/SET the values of its respective inverse

makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve is a function that gets the value from a matrix sent as a parameter and checks if its inverse has been cached already. If it is cached it will return the result from the cache.
# If the inverse is not cached it will calculate it and store it on the same variable that was used to call the function.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i))
    {
        message("Inverse found! Getting data from cache...")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

##Test Results

#> m <- matrix(c(4,7,2,6), nrow = 2, ncol = 2, byrow = TRUE)
#> m
#[,1] [,2]
#[1,]    4    7
#[2,]    2    6

#> z <- makeCacheMatrix(m)
#> z$get()
#[,1] [,2]
#[1,]    4    7
#[2,]    2    6

#> cacheSolve(z)
#[,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4

#> cacheSolve(z)
#Inverse found! Getting data from cache...
#[,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4